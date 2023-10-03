:- module(loader, [load_kb/2, get_coverage_and_clear/0, clear_dynamic/0]).
/** <module> loader

This module manages the execution for the instrumented knowledge-base.

*/
:- use_module(instrumenter, [instrument/3]).
:- use_module(logger, [init/2, coverage/1, clean/0, stop/0]).
:- use_module(visualizer, [visualize/1]).

:- dynamic not_first/0.

%% init(+File : string, +Commands: list).
%
% Succeeds after running the instrumenter over a .pl file, and feeding the 
% results to the logger.
%
% @param File      The string representing the name/path of the .pl file.
% @param Commands  The commands to be passed on to the instrumenter.
load_kb(File, Commands) :-
	clear_dynamic,
    instrumenter : instrument(File, KB, Commands),
    logger : init(Commands, KB),
    !.

%% get_coverage_and_clear.
%
% Succeeds after printing the code coverage currently logged, and clearing all
% logs.
get_coverage_and_clear :-
    get_coverage,
    clear_logs,
    !.

%% get_coverage.
%
% Succeeds after recieving results from the logger and printing them with the
% visualizer.
get_coverage :-
    logger : coverage(Terms),
    visualizer : visualize(Terms).

%% clear_logs.
%
% Succeeds after clearing any logs used during execution, preparing the logger
% for any further queries.
clear_logs :-
    (not_first -> retract(not_first);true),
    logger : clean.

%% clear_dynamic.
%
% Succeeds after deleting all logs, a new load will have to be called in order
% to continue generating coverage results.
clear_dynamic :-
    (not_first -> retract(not_first);true),
    logger : stop.
