:- module(launcher, [insert_launcher_calls/5]).
/** <module> launcher

This module adds launcher code to an terms that have been processed by the 
instrumenter, adding function calls that allow processing of more fine-grained
details by the coverage tools.

*/
:- use_module(utils, [rearrange/4, univ_to/2, zip/6]).
:- use_module(instrumenter, [process_term/4, log_term/7]).

%% configure_launcher(+Terms : list, +Term_signatures: list, +Options: list, -Launcher_terms: list).
%
% Succeeds after adding additional configuration predicates to a list of 
% prolog terms. These predicates can have multiple layers to measure different
% things and eventually finish by calling the original terms.
%
% There are three possible configuration layers to add, each is selected using a
% keyword option:
%  - [cmd]:     The laucher option adds an initial layer allowing the coverage  
%               to be measured directly from the prolog terminal, by simply  
%               typing a term in the same way as if there was no code coverage 
%               running.
%  - [ground]:  The ground layer measures the ground terms in each predicate 
%               before the execution is complete. This gives an indication of 
%               what predicate parameters are in/out.
%  - [branch]:  The branch layer measures the possible coverage for each clause
%               branch in a predicate call, independently of whether or not the 
%               clause ends up being chosen for execution.
%
% Due to the structure of the added layers, the [cmd] layer must always be the 
% first layer in the execution if present, and therefore must be the last to be 
% added.
%
% @param Terms             The list of terms of terms to process.
% @param Term_signatures   The list of signatures for all terms in the 
%                          form <ID>-<Functor>/<Arity>.
% @param Options           The list of options for the launcher, either [cmd]
%                          or [].
% @param Launcher_terms    The original Terms with the launcher instrumentation
%                          added at the start of the list.
insert_launcher_calls(Options, Types, IDs, KB, Launcher_terms) :-
    reorder_options(Options, Options_reordered),
    instrument_launchers(Options_reordered, Types, IDs, KB, Launcher_terms).

reorder_options(Options, Options_reordered) :-
    append(A, [cmd | B], Options),
    !,
    append([[cmd], A, B], Options_reordered).
reorder_options(Options, Options).

%% instrument_with_option(+Terms_and_signatures : term, +Names : list, +Options: list, +Depth: int).
%
% Succeeds after instrumenting every predicate following the options in 
% <Options>. For each option a new set of predicates linked to the next depth.
% The original predicates are not considered, only adding predicates to measure 
% additional parameters.
%
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Signatures             The list of predicate names in the form 
%                               <Functor> / <Arity>.
% @param Options                List of user-defined options describing what 
%                               terms to add.
% @param Depth                  The current instrumenter depth.
instrument_launchers(Options, Types, IDs, KB, Instrumented) :-
    instrument_launchers(Options, Types, IDs, KB, 0, Inst_unflat),
    flatten(Inst_unflat, Instrumented).

instrument_launchers([Option | Ot], Types, IDs, KB, Depth, [Inst | It]) :-
    names_to(Option, Types, IDs, KB, Depth, Inst),
    Next_depth is Depth + 1,
    instrument_launchers(Ot, Types, IDs, KB, Next_depth, It).

instrument_launchers([], _, _, KB_Types - _ - KB_Structures, Depth, Base_terms) :-
    maplist(mark_term(Depth), KB_Types, KB_Structures, Base_terms).

%% create_launcher_predicates(+Signatures : list, -Names_unique: list).
%
% Succeeds after converting <Signatures> of all clauses into a list of predicate
% names.
% 
% Due to multiple clauses per predicate, clause names are sorted to remove 
% duplicates.
%
% @param Signatures   The list of predicate clause signatures in the form 
%                     <Counter> - <Functor> / <Arity>.
% @param Names_unique The list of predicate names in the form 
%                     <Functor> / <Arity>.
get_predicate_names(Signatures, Names_unique) :-
    utils:rearrange(Signatures, _Counter - Functor / Arity, 
       Functor / Arity, Names),
       sort(Names, Names_unique).
 
%% names_to(+Terms_and_signatures : term, +Names : list, +Option: term, +Depth: int, -Instrumented_terms: list).
%
% Succeeds after instrumenting every name in <Names> according to a given 
% option.
%
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Signatures             The list of predicate names in the form 
%                               <Functor> / <Arity>.
% @param Option                 User-defined option describing what to instrument
% @param Depth                  The current instrumenter depth.
% @param Instrumented_terms     The list of instrumented terms.
names_to(Option, Types, IDs, _ - KB_IDs - KB_Structures, Depth, Launchers) :-
    maplist(name_to(Option, KB_IDs - KB_Structures, Depth), Types, IDs, Launchers).

%% names_to_launchers(+Names : list, -Launchers: list, +Depth: int).
%
% Succeeds after creating launcher predicates for every name in <Names>.
% 
% Three launcher predicates are created for every name, each represent a
% different behaviour:
%   - First:  When the predicate is not the initial query and simply redirects
%             to the next instrumenter depth.
%   - Second: When the predicate is the initial query and succeeds when 
%             redirecting.
%   - Third:  When the predicate is the initial query and fails when 
%             redirecting.
%
% @param Signatures   The list of predicate names in the form 
%                     <Functor> / <Arity>.
% @param Launchers    The list of launchers, 3x the original in size.
% @param Depth        The current instrumenter depth.
name_to(cmd, _, Depth, Type, ID, [First, Second, Third]) :-
    launcher_step_marks(ID - Depth, Head, Call),
    instrumenter:log_term(front, Type, (loader:not_first,!), Head - Call, _, First, _),
    instrumenter:log_term(both, Type, (assertz(loader:not_first)) / (!,loader:get_coverage_and_clear), Head - Call, _, Second, _),
    instrumenter:log_term(front,  Type, (loader:get_coverage_and_clear,fail), Head - (fail), _, Third, _).

name_to(ground, _, Depth, Type, ID, Launcher) :-
    launcher_step_marks(ID - Depth, Head, Call, Empty),
    instrumenter:log_term(front, Type, (logger:coverage(ground, ID, Empty)), Head - Call, _, Launcher, _).

name_to(branch, Data, Depth, Type, ID, Launcher) :-
    launcher_step_marks(ID - Depth, Head, Call, Empty),
    get_clause_heads(ID, Data, Heads),
    instrumenter:log_term(front, Type, (logger:coverage(branch, ID, Heads - Empty)), Head - Call, _, Launcher, _).

%% names_to_branches(+Terms_and_signatures: term, +Names : list, -Branches: list, +Depth: int).
%
% Succeeds after creating a Branch predicate for every name in <Names>. Each
% Branch predicate will contain all possible clauses to unify for the predicate
% to test it's branching capacity.
%
% Similarly to other instrumenter functions, each Branch predicate redirects to
% the next instrumenter depth.
% 
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Signatures             The list of predicate names in the form 
%                               <Functor> / <Arity>.
% @param Branches               The list of Branch predicates.
% @param Depth                  The current instrumenter depth.

%% get_clause_heads(+Signature : term, +Terms_and_signatures : term, -Clause_heads: list).
%
% Succeeds after finding all clause heads in the term list of 
% <Terms_and_signatures> that match in name with <Signature>. In other words,
% finds every clause in the program belonging to a predicate with name 
% <Signature>. 
% 
% @param Signature              The predicate name in the form <Functor> / 
%                               <Arity>.
% @param Terms_and_signatures   A <list>-<list> representing all predicate 
%                               clauses in the program and their signatures.
% @param Clause_heads           The list of clauses belonging to <Signature>.
get_clause_heads(ID, IDs - Structures, Heads) :-
    utils:zip(IDs, Structures, ID_1, Head_1 - _, ID_1 - Head_1, Term_heads),
    exclude(\=(ID - _), Term_heads, Heads_filtered),
    utils:rearrange(Heads_filtered, _ - Head, Head, Heads).

%% mark_terms(+Terms : list, -Marked_terms: list, +Depth: int).
%
% Succeeds after marking the head of each term in the list to 
% indicate they belong to the instrumenter.
%
% @param Terms          The list of terms to be modified.
% @param Marked_terms   The resulting list after modifications.
% @param Depth          The depth of the marks to add.
mark_term(_, ignore, Head - _, Head).
mark_term(Depth, Type, Head - Body, Marked_term) :-
    add_instrumenter_marks(Head, Marked_head, Depth),
    instrumenter:process_term(Marked_term, Type, _, Marked_head - Body).

%% add_instrumenter_marks(+Term : term, -Term_renamed: term, +Amount: int).
%
% Succeeds after renaming the functor of <Term>, adding <Amount> amount of 
% '_i' instrumenter indication marks after the functor name.
% 
% Terms that are atoms are similarly renamed.
%
% @param Term           The term to be modified.
% @param Term_renamed   The <Term> with <Amount> '_i's added.
% @param Amount         The amount of '_i's to add.
launcher_step_marks(Functor / Arity - Depth, This, Next) :-
    launcher_step_marks(Functor / Arity - Depth, This, Next, _).

launcher_step_marks(Functor / Arity - Depth, This, Next, Predicate) :-
    functor(Predicate, Functor, Arity),
    add_instrumenter_marks(Predicate, This, Depth),
    Past_depth is Depth + 1,
    add_instrumenter_marks(Predicate, Next, Past_depth).

add_instrumenter_marks(Term, Term_renamed, Amount) :-
    Term =.. [Functor | Args],
    length(Mark_list, Amount), 
    maplist(=('_i'),Mark_list), 
    Concat_list = [Functor | Mark_list], 
    atomic_list_concat(Concat_list, Functor_renamed),
    Term_renamed =.. [Functor_renamed | Args].