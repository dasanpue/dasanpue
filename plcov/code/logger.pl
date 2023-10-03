:- module(logger, [init/2, coverage/1, clean/0, stop/0]).
/** <module> logger

This module is in charge of asserting and managing the calls to pred_start

*/
:- dynamic coverage/2.
:- dynamic grounds/2.
:- dynamic branches/2.

:- use_module(utils, [rearrange/4]).
:- use_module(kb, [
    map_over/5, 
    empty/1, 
    init_kb/1, 
    pop_term/3, 
    peek_term/2, 
    funct/2, 
    cl_num/2, 
    funct/3]).

%% init(+Term_signatures : list)
%
% Succeeds after asserting a 'coverage' term to the knowledge base, acting
% as a counter for pred_start called with the same parameters.
%
% @param Term_signatures   The list of signatures for all terms to handle
%                          terms in the form <ID>-<Functor>/<Arity>.
init(Commands, KB) :-
    filter_kb(KB, KBF),
    kb : map_over(
        Term, 
        logger : create_coverage_predicate(Commands, Term, Predicate), 
        Predicate,
        Predicates,
        KBF),
    maplist(assertz, Predicates).


filter_kb(KB, KBF) :-
    kb : empty(KB),
    kb : init_kb(KBF).
filter_kb(KB, KBF) :-
    kb : pop_term(KB, Term, KB_popped),
    kb : peek_term(KB_popped, Term_next),
    kb : funct(Funct_t, Term),
    kb : funct(Funct_tn, Term_next),
    Funct_t \= Funct_tn,
    kb : pop_term(KBF, Term, KBF_popped),
    filter_kb(KB_popped, KBF_popped).
filter_kb(KB, KBF) :-
    kb : pop_term(KB, Term, KB_popped),
    kb : empty(KB_popped),
    kb : pop_term(KBF, Term, KBF_popped),
    filter_kb(KB_popped, KBF_popped).
filter_kb(KB, KBF) :-
    kb : pop_term(KB, _, KB_popped),
    filter_kb(KB_popped, KBF).

create_coverage_predicate(Commands, Term, Predicate) :-
    kb : funct(Funct, Term),
    kb : cl_num(Occurances, Term),
    kb : funct(Funct, Name, Arity),

    numlist(1, Occurances, Increments),
    utils:rearrange(Increments,
        Number,
        Number - 0,
        Empty_clauses),
    
    add_option([], Commands, ground, ground - [], CL_1),
    add_option(CL_1, Commands, branch, branch - [], CL_2),
    CL_3 = [clause - Empty_clauses | CL_2],

    Predicate = coverage(Name / Arity, CL_3).

add_option(L, Commands, Contains, E, [E | L]) :- member(Contains, Commands).
add_option(L, _, _, _, L).

%% pred_start(+Number : number, +Name : '/'(string, int)).
%
% Succeeds after asserting a 'clause' term to the count in the knowledge base.
%
% @param Number      The ID of the predicate.
% @param Name        The Functor/Arity of the predicate head.

%% pred_ground(+Predicate : Term).
%
% Succeeds after asserting a 'grounds' term to the count in the knowledge base.
%
% @param Predicate   The head of clause that has matched with a query.

%% get_ground_indicators(+Predicate : Term, -Predicate_with_indicators : Term).
%
% Succeeds after substituting arguments in a term in the form <Name>(arg1,...) 
% with an indicator of whether they are grounded.
%
% The output will show g if the argument is grounded and ng if it is not.
%
% @param Predicate   The head of clause that has matched with a query.
% @param Predicate   The head of clause with 'g' and 'ng' as arguments.
get_ground_indicators(Predicate, Pred_with_indicators) :-
    functor(Predicate, _, Arity), 
    Predicate =.. [Head | Args], 
    length(Indicators, Arity), 
    maplist(detect_ground, Args, Indicators), 
    Pred_with_indicators =.. [Head | Indicators].

detect_ground(P, g) :- ground(P), !.
detect_ground(_, ng).

%% pred_branch(+Branch_list : list, +Predicate : Term).
%
% Succeeds after asserting a 'branches' term to the count in the knowledge base.
% The first argument of this term is a list indicating if the clause number 
% corresponding to the list index matches with the <Predicate> term with a 1 if 
% there is a match and a 0 if not.
%
% @param Branch_list    A list of clauses for a given predicate
% @param Predicate      The query term to test for unification.
unifies(Predicate, Branch, 1) :- unifiable(Predicate, Branch, _), !.
unifies(_, _, 0).

%% add_to_asserted(+Name : atom, +Content : term).
%
% Succeeds after asserting a term to the knowledge base, with a counter 
% storing the times the predicate has been called with the same parameters.
%
% If this specific parameter combination has already been asserted, adds
% 1 to the counter storing it.
%
% @param Name        The Functor of a predicate.
% @param Content     The first argument of the predicate.

coverage(Type, ID, Indicator) :-
    Predicate = coverage(ID, Coverage_List),
    call(Predicate),

    replace_key(Coverage_List, Type, Key_Counts, Key_Counts_Updated, Coverage_List_Updated),
    
    key(Type, Indicator, Key),
    update_key(Key, Key_Counts, Key_Counts_Updated),
    
    retract(Predicate),
    Replacement = coverage(ID, Coverage_List_Updated),
    assertz(Replacement).

key(clause, Indicator, Indicator).
key(ground, Indicator, Key) :-
    get_ground_indicators(Indicator, Key).
key(branch, Branches - Predicate, Key) :-
    maplist(unifies(Predicate), Branches, Key).

update_key(Key, Key_Values, Key_Values_Updated) :-
    replace_key(Key_Values,Key, Count, Increment, Key_Values_Updated),
    !,
    Increment is Count + 1.
update_key(Key, Key_Values, [Key - 1 | Key_Values]).

replace_key(Key_Values, Key, Value, New_Value, Key_Values_Updated) :-
    append(L1, [Key - Value | L2], Key_Values),
    append(L1, [Key - New_Value | L2], Key_Values_Updated).

%% coverage(-Covered_terms: list).
%
% Succeeds after setting Covered_terms to the list of terms with 
% number of calls to pred_start.
%
% @param Covered_terms  The list of terms with number of calls in the format
% <CallNumber> - <Functor> / <Arity> / <Number>
coverage(Covered_terms) :-
    findall(ID - Coverage, 
        coverage(ID, Coverage),
        Covered_terms).

%% clean.
%
% Succeeds after setting all coverage/2 terms back to 0, restarting the logger
% counter.
%
% All existing asserted coverage/2 terms are retracted and asserted with the 
% counter set to 0.
clean :-
    findall(ID - Value, logger:coverage(ID, Value), Bag),
    maplist(reset_coverage, Bag, Predicates),
    retractall(coverage(_,_)),
    maplist(assertz, Predicates).

reset_coverage(ID - Measurements, coverage(ID, Reset)) :-
    maplist(reset, Measurements, Reset).

reset(clause - List,clause - Reset) :-
    utils:rearrange(List, K-_, K-0, Reset).
reset(branch - _,branch - []).
reset(ground - _,ground - []).
reset(_,[]).

%% stop.
%
% Succeeds after retracting all asserted coverage/2 terms.
stop :-
    retractall(logger:coverage(_,_)).

print_array([]).
print_array([Head | Tail]) :-
    write(Head),       
    write(', '),       
    print_array(Tail).