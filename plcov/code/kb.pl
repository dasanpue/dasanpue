:- module(kb, [
    read_and_process/2, 
    empty/1, 
    pop_term/3,
    peek_term/2,
    funct/2, 
    funct/3,
    head/2, 
    cl_num/2, 
    type/2,
    join/2, 
    mark_term/3, 
    mark_terms/3, 
    init_kb/1, 
    init_term/1,
    pop_terms/3,
    unify_remaining/2,
    instrument_term/4,
    filter_by_funct/3,
    write_to_file/2,
    clear_head/2,
    map_over/5]).
 

:- use_module(utils, [call_over_file/4]).

% Remove/Add module calls for process_term

% ------------------------------------------------------------------------------
init_kb([functors(_), cl_nums(_), types(_), structures(_)]).
init_term([functor(_), cl_num(_), type(_), structure(_)]).

functs(Functors, [functors(Functors) | _]).
funct(Functor, [functor(Functor) | _]).
funct(Name / Arity, Name, Arity).

cl_nums(Cl_nums, [_, cl_nums(Cl_nums) | _]).
cl_num(Cl_num, [_, cl_num(Cl_num) | _]).

types(Types, [_, _, types(Types) | _]).
type(Type, [_, _, type(Type) | _]).

structures(Structures, [_, _, _, structures(Structures)]).
structure(Structure, [_, _, _, structure(Structure)]).
structure(Head - Body, Head, Body).
head(Head, Term) :- structure(Head - _, Term).
body(Body, Term) :- structure(_ - Body, Term).

kb_data([Functors, Cl_nums, Types, Structures], KB) :-
    functs(Functors, KB),
    cl_nums(Cl_nums, KB),
    types(Types, KB),
    structures(Structures, KB).

pop_term([
            functors([F|Ft]), 
            cl_nums([C|Ct]), 
            types([T|Tt]), 
            structures([S|St])
        ], 
        [
            functor(F), 
            cl_num(C), 
            type(T), 
            structure(S)
        ], 
        [
            functors(Ft), 
            cl_nums(Ct), 
            types(Tt), 
            structures(St)
        ]).

peek_term(KB, Term) :-
    pop_term(KB, Term, _).

pop_terms(KB, [Term | Terms], KBP) :-
    pop_term(KB, Term, KB_popped),
    pop_terms(KB_popped, Terms, KBP).
pop_terms(KB, [], KB).

empty([functors([]), cl_nums([]), types([]), structures([])]).

join([], KB_init) :- empty(KB_init).
join([KB | KBt], KBs_joined) :-
    join(KBt, KBs),
    kb_data(Data_elem, KB),
    kb_data(Data_existing, KBs),
    kb_data(Data_out, KBs_joined),
    maplist(append, Data_elem, Data_existing, Data_out).

% should have a better name, specify unifications
unify_remaining([], []) :- !.
unify_remaining([H|T1], [H|T2]) :- unify_remaining(T1, T2).
unify_remaining([_|T1], [_|T2]) :- unify_remaining(T1, T2).
copy_remaining([], []) :- !.
copy_remaining([H1|T1], [H2|T2]) :- (var(H1); var(H2)), H1 = H2, copy_remaining(T1, T2).
copy_remaining([_|T1], [_|T2]) :- copy_remaining(T1, T2).

filter_by_funct(_, KB, KB_filtered) :- empty(KB), init_kb(KB_filtered).
filter_by_funct(Funct, KB, KBF) :-
    pop_term(KB, Term, KB_popped),
    funct(Funct, Term),
    pop_term(KBF, Term, KBF_popped),
    filter_by_funct(Funct, KB_popped, KBF_popped).
filter_by_funct(Funct, KB, KBF) :-
    pop_term(KB, _, KB_popped),
    filter_by_funct(Funct, KB_popped, KBF).

heads([], KB) :- empty(KB).
heads([Head | Heads], KB) :-
    pop_term(KB, Term, KB_popped),
    head(Head, Term),
    heads(Heads, KB_popped).

map_over(_, _, _, [], KB) :- empty(KB).
map_over(Term, Goal, Result, [Result | Results], KB) :-
    copy_term((Term, Goal, Result), (Term_n, Goal_n, Result_n)),
    pop_term(KB, Term, KB_popped),
    call(Goal),
    map_over(Term_n, Goal_n, Result_n, Results, KB_popped).

generate(_, _, _, KB_start, KB_end) :- empty(KB_start), init_kb(KB_end).
generate(Term, Goal, Result, KB_start, KB_end) :- 
    copy_term((Term, Goal, Result), (Term_n, Goal_n, Result_n)),
    pop_term(KB_start, Term, KBS_popped),
    call(Goal),
    pop_term(KB_end, Result, KBE_popped),
    generate(Term_n, Goal_n, Result_n, KBS_popped, KBE_popped).
% ------------------------------------------------------------------------------

read_and_process(File, KB) :-
    utils:call_over_file(File, read_terms, read, File_terms),

    term_compositions(File_terms, [Functors - Types - Structures]),
    clause_nums(Functors, Cl_nums),

    init_kb(KB),
    functs(Functors, KB),
    cl_nums(Cl_nums, KB),
    types(Types, KB),
    structures(Structures, KB).

%

instrument(_, [], KB, KBI) :- empty(KB), init_kb(KBI).
instrument(Position, [Goal | Goals], KB, KBI) :-
    pop_term(KB, Term, KB_popped),
    instrument_term(Position, Goal, Term, Instrumented_term),
    pop_term(KBI, Instrumented_term, KBI_popped),
    instrument(Position, Goals, KB_popped, KBI_popped).

instrument_term(_, Goal, Term, Instrumented_term) :-
    type(fact, Term),
    head(Head, Term),
    
    type(rule, Instrumented_term),
    body(Goal, Instrumented_term),
    head(Head, Instrumented_term),
    
    unify_remaining(Term, Instrumented_term).

instrument_term(Position, Goal, Term, Instrumented_term) :-
    type(rule, Term),
    head(Head, Term),
    body(Body, Term),

    arrange(Position, Goal, Body, Instrumented_body),
    
    head(Head, Instrumented_term),
    body(Instrumented_body, Instrumented_term),
    
    unify_remaining(Term, Instrumented_term).

instrument_term(Position, Goal, Term, Instrumented_term) :-
    type(dcg, Term),
    head(Head, Term),
    body(Body, Term),

    arrange(Position, {Goal}, Body, Instrumented_body),
    
    head(Head, Instrumented_term),
    body(Instrumented_body, Instrumented_term),
    
    unify_remaining(Term, Instrumented_term).

instrument_term(_, _, Term, Term) :-
    type(ignore, Term).

arrange(front, A, B, (A, B)).
arrange(back, A, B, (B, A)).
arrange(both, A / C, B, (A, B, C)).
arrange(replace, A, _, A).
%
term_compositions(Terms, [Functors - Types - Structures]) :-
    maplist(term_composition, Terms, Types, Functors, Structures).

term_composition(Predicate, fact, Functor, Structure) :-
    functor(Predicate, Name, Arity),
    not(current_op(_,_,Name)),
    !, % Y
    kb : funct(Functor, Name, Arity),
    kb : structure(Structure, Predicate, true).
term_composition(Predicate, rule, Functor, Structure) :-
    functor(Predicate, :-, 2), 
    !, % Y
    arg(1, Predicate, Head),
    arg(2, Predicate, Body),
    kb : funct(Functor, Name, Arity),
    kb : structure(Structure, Head, Body),
    functor(Head, Name, Arity).
    
term_composition(Predicate, dcg, Functor, Structure) :-
    functor(Predicate, -->, 2),
    !, % Y
    arg(1, Predicate, Head),
    arg(2, Predicate, Body),
    kb : funct(Functor, Name, Arity),
    kb : structure(Structure, Head, Body),
    functor(Head, Name, Arity).
term_composition(Predicate, ignore, none, Structure) :-
    kb : structure(Structure, Predicate, fail).
%
clause_nums(Functors, Clause_nums) :-
    reduce_count(Functors, Functors_counted),

    unfold_counted(Functors_counted, Clause_nums).


reduce_count(L1, L2) :- reduce_count(L1, L2, 1).
reduce_count([H, H | T], Red, Count) :-
    !,
    Next is Count + 1,
    reduce_count([H | T], Red, Next).
reduce_count([H | T], [H - Count | Red], Count) :-
    reduce_count(T, Red, 1).
reduce_count([], [], _).
unfold_counted(Input, Output) :-
   maplist(unfold_term, Input, Output_2D),
   flatten(Output_2D, Output).

unfold_term(_-Count, Increments) :-
   numlist(1, Count, Increments).
%
mark_terms(Mark, KB, KBM) :-
    generate(Term, mark_term(Mark, Term, Term_marked), Term_marked, KB, KBM).

clear_head(Term, Term_cleared) :-
    head(Head_term, Term),
    functor(Head_term, Name, Arity),
    length(Args, Arity),
    Head_cleared =.. [Name | Args],
    head(Head_cleared, Term_cleared),
    copy_remaining(Term_cleared, Term).

mark_term(_, Term, Term) :-
    type(ignore, Term).
mark_term(Mark, Term, Term_marked) :-
    funct(Funct, Term),
    funct(Funct, Name, Arity),
    head(Head, Term),
    body(Body, Term),
    Head =.. [Name | Args],
    atom_concat(Name, Mark, Name_marked),
    Head_marked =.. [Name_marked | Args],
    
    funct(Funct_marked, Name_marked, Arity),
    funct(Funct_marked, Term_marked),
    head(Head_marked, Term_marked),
    body(Body, Term_marked),
    unify_remaining(Term_marked, Term).
%

raw_terms(Terms, KB) :-
    map_over(Term, raw_term(Term, Term_raw), Term_raw, Terms, KB).
raw_term(Term, Term_raw) :-
    type(Type, Term),
    funct(Funct, Term),
    structure(Structure, Term),
    term_composition(Term_raw, Type, Funct, Structure).

write_to_file(File, KB) :-
    raw_terms(Terms, KB),
    utils:call_over_file(File, write_terms, write, Terms).