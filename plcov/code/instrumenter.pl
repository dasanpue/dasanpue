:- module(instrumenter, [instrument/3]).
/** <module> instrumenter

This module instruments a .pl file, adding function calls to build a basis for
code coverage tools to use.

When additional options are given, the <launcher> module is called to append
additional instrumentation.

*/
:- use_module(utils, [call_over_file/4]).
:- use_module(kb, [
   read_and_process/2, 
   empty/1, 
   pop_term/3, 
   funct/2, 
   head/2, 
   cl_num/2, 
   type/2,
   join/2, 
   mark_term/3, 
   mark_terms/3, 
   init_kb/1,
   pop_terms/3,
   unify_remaining/2,
   instrument_term/4,
   filter_by_funct/3,
   write_to_file/2,
   map_over/5]).



instrument(File, KB, Options) :-

   kb : read_and_process(File, KB),

   coverage_goals(Goals_cov, KB),

   kb : instrument(front, Goals_cov, KB, KBI),
   
   launchers_options(Options, KBI, KBL),

   kb : write_to_file('.temp.pl', KBL).


coverage_goals(Goals, KB) :-
   kb : map_over(
      Term, 
      instrumenter:coverage_goal(Term, Coverage_goal), 
      Coverage_goal, 
      Goals, 
      KB).

coverage_goal(Term, logger : coverage(clause, Functor, Clause)) :-
   kb : funct(Functor, Term),
   kb : cl_num(Clause, Term).



reorder_options(Options, Options_reordered) :-
   append(A, [cmd | B], Options),
   !,
   append([[cmd], A, B], Options_reordered).
reorder_options(Options, Options).
%
launchers_options(Options, KB, KBL) :-
   reorder_options(Options, Options_reordered),
   launchers_options(Options_reordered, KB, KBL, 0).


launchers_options([Option | Ot], KB, KBLs_joined, Depth) :-
   launchers(Option, Depth, KB, KBL),
   Next_depth is Depth + 1,
   launchers_options(Ot, KB, KBLs, Next_depth),
   kb : join([KBL, KBLs], KBLs_joined).

launchers_options([], KB, KB_marked, Depth) :-
   generate_mark(Depth, Mark),
   kb : mark_terms(Mark, KB, KB_marked).

generate_mark(0, '').
generate_mark(Number, Mark) :-
   Number > 0,
   atom_concat('_', Number, Mark).




launchers(_, _, KB, KBL) :- 
   kb : empty(KB), 
   kb : init_kb(KBL).
launchers(Option, Depth, KB, KBL) :-
   kb : pop_term(KB, Term, KB_popped),
   kb : cl_num(1, Term),
   kb : type(Type, Term),
   Type \= ignore,
   !,
   term_launcher(Option, Depth, Term, KB, Launchers),
   kb : pop_terms(KBL, Launchers, KBL_popped),
   launchers(Option, Depth, KB_popped, KBL_popped).
launchers(Option, Depth, KB, KBL) :-
   kb : pop_term(KB, _, KB_popped),
   launchers(Option, Depth, KB_popped, KBL).

term_launcher(Option, Depth, Term, KB, Launchers) :-
   kb : clear_head(Term, Term_empty),

   generate_mark(Depth, LMark),
   kb : mark_term(LMark, Term_empty, Term_lmark),

   Next_depth is Depth + 1,
   generate_mark(Next_depth, LLMark),
   kb : mark_term(LLMark, Term_empty, Term_llmark),
   kb : head(Call, Term_llmark),
   
   kb : body(Call, Launcher),
   kb : unify_remaining(Term_lmark, Launcher),
   
   term_launcher_instrument(Option, Term, Launcher, KB, Launchers).

term_launcher_instrument(cmd, _Term, Launcher, _KB, [Launcher_1, Launcher_2, Launcher_3]) :-
   kb : instrument_term(
      front, (
         loader:not_first,
         !
      ), 
      Launcher, 
      Launcher_1),
   kb : instrument_term(
      both, (
         assertz(loader:not_first)
      ) / (
         !,
         loader:get_coverage_and_clear
      ), 
      Launcher, 
      Launcher_2),
   kb : instrument_term(
      replace, (
         loader:get_coverage_and_clear,
         fail
      ), 
      Launcher, 
      Launcher_3).

term_launcher_instrument(ground, Term, Launcher, _KB, [Launcher_1]) :-
   kb : funct(Funct, Term),

   kb : head(Head_term, Term),
   kb : head(Head_launcher, Launcher),
   functor(Head_term, Name, _),
   Head_launcher =.. [_ | Args],
   Head =.. [Name | Args],

   kb : instrument_term(
      front, (
         logger:coverage(ground, Funct, Head)
      ), 
      Launcher, 
      Launcher_1).

term_launcher_instrument(branch, Term, Launcher, KB, [Launcher_1]) :-
   kb : funct(Funct, Term),

   kb : head(Head_term, Term),
   kb : head(Head_launcher, Launcher),
   functor(Head_term, Name, _),
   Head_launcher =.. [_ | Args],
   Head =.. [Name | Args],

   kb : filter_by_funct(Funct, KB, KB_clauses),
   kb : heads(Clause_heads, KB_clauses),
   kb : instrument_term(
      front, (
         logger:coverage(branch, Funct, Clause_heads - Head)
      ), 
      Launcher, 
      Launcher_1).


% @param Clause_heads           The list of clauses belonging to <Signature>.

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

%% mark_terms(+Terms : list, -Marked_terms: list, +Depth: int).
%
% Succeeds after marking the head of each term in the list to 
% indicate they belong to the instrumenter.
%
% @param Terms          The list of terms to be modified.
% @param Marked_terms   The resulting list after modifications.
% @param Depth          The depth of the marks to add.

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