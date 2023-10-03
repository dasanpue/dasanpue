#!/usr/bin/env swipl

:- initialization main.

main(Argv) :- length(Argv, 0).
main(Argv) :-
    argv_options(Argv, _PositionalArgs, Options),

    find(Options, file_cov, File, err('missing file')),
    find(Options, coverage, Opt, [cmd]),
    find(Options, goal, Goal, err('no goal specified')),
    load(File, Opt),
    call(Goal),
    unload,
    halt.

opt_type(f, file_cov,  file).
opt_type(file, file_cov,  file).
opt_type(g, goal,  term).
opt_type(goal, goal,  term).
opt_type(c, coverage,  term).
opt_type(coverage, coverage,  term).

find([], _, _, err(Msg)) :- write(Msg), nl, halt.
find([], _, Default, Default).
find([Option | _], Type, Content, _) :- 
    functor(Option, Type, _),
    !,
    arg(1, Option, Content).
find([_ | Options], Type, Content, Default) :- find(Options, Type, Content, Default).

:- use_module(loader, [load_kb/2, get_coverage_and_clear/0, clear_dynamic/0]).

:- use_module(utils, [call_over_file/4]).

load(File) :-
    loader: load_kb(File, [cmd]),
    ['.temp.pl'].

load(File, Commands) :-
    loader: load_kb(File, Commands),
    ['.temp.pl'].

query(File) :-
    utils:call_over_file(File, read_terms, read, Terms),
    maplist(call, Terms),
    !,
    coverage.

coverage :-
    loader: get_coverage_and_clear.

unload :-
    loader: clear_dynamic.