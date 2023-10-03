:- module(visualizer, [visualize/1]).
/** <module> logger

This module is in charge of visualizing the function calls recorded by
the logger.

*/
:- use_module(utils, [rearrange/4, transpose/2]).

%% visualize(+Signatures : list).
%
% Succeeds after calculating percentages, creating a table, ordering and 
% printing <Signatures>.
%
% @param Term_signatures   The list of signature occurances given by the logger
%                          in the form <Occurances>-<Functor>/<Arity>/<Number>.



    % percentages(Signatures, G_percentages, signature_to_global),
    % percentages(Signatures, L_percentages, signature_to_local),
    
    % utils:rearrange(Signatures, 
    %     Occurances - Functor / Arity / Number, 
    %     Number - Functor / Arity / Occurances,
    %     Signature_table),
    % maplist(add_column, Signature_table, G_percentages, G_percentage_table),
    % maplist(add_column, G_percentage_table, L_percentages, Print_table),
    
    % header,
    % pretty(Signature_table).

%% percentages(+Signatures : list, -Percentages : list, :Signature_to_id : term).
%
% Succeeds after calculating percentages for each signature as occurrances of
% the term created by applying <Signature_to_id> to the signature. 
%
% @param Signatures         The list of signature occurances given by the logger 
%                           in the form <Occurances>-<Signature>.
% @param Percentages        The list of % occurances for each signature after
%                           applying <Signature_to_id>.
% @param Signature_to_id    A predicate in the form <Signature_to_id>(In, Out)
%                           converting a signature in the form <Functor>/
%                           <Arity>/<Number> to an ID.

%% find_and_add(+Element : term, +Amount : integer, _DCG_A : list, _DCG_B : list).
%
% Succeeds after finding <Element> in the DCG, and updating it's count
% by <Amount>.
% If <Element> is not present, it add's a new (<Element>, <Count>) to the DCG,
% with <Count> = <Amount>.
%
% @param Element    The ID of an element to find in the DCG.
% @param Amount     The number by which to increment the current count for
%                   <Element>.


%% calculate_percent(+Element : term, +Amount : integer, -Percent : float, _DCG_A : list, _DCG_B : list).
%
% Succeeds after calculating the proportion of <Amount> over the total
% given in the DCG for <Element>.
% If the total in the DCG is 0, <Percent> = 0 (0% of 0 is 0).
%
% @param Element    The ID of an element to find in the DCG.
% @param Amount     The numerator of the proportion.
% @param Percent    The result of <Amount> over the given total in the DCG.


visualize(Coverage) :-
    %write(Coverage),
    table_dimensions(predicate-Coverage, D),
    level(predicate-Coverage, Length_info),
    process(D, F, predicate-Coverage, Length_info), 
    maplist(process_format, F,For_strings,For_argss), 
    maplist(format, For_strings, For_argss),
    !.

entrada(predicate - [	
    father/2   - [clause-[1-0,2-0,3-0,4-0],branch-[],ground-[]], 
    ancestor/2 - [clause-[1-1,2-0],branch-[[1,1]-1],ground-[ancestor(g,g)-1]], 
    parent/2   - [clause-[1-1,2-0],branch-[[1,1]-1],ground-[parent(g,g)-1]], 
    mother/2   - [clause-[1-1,2-0,3-0,4-0],branch-[[1]-1],ground-[mother(g,g)-1]]
    ]).

level(Coverage, Named_maxes) :-
    is_beforefore(Coverage),
    !,
    Coverage = _ - Counts,
    maplist(collect_counts, Counts, Named_maxes).

level(Coverage, Data) :- 
    next_level(Coverage, Data).
    
next_level(_ - Elements, Joined_data) :- 
    maplist(level, Elements, Datas),
    utils:transpose(Datas, D1),
    keyed_maxes(D1, Joined_data).

keyed_maxes([],[]).
keyed_maxes([List | Dt], [Key - Max | Kt]) :-
    utils:rearrange(List, Key - Count, Count, Counts),
    max_list(Counts, Max),
    keyed_maxes(Dt, Kt).

collect_counts(Coverage, Name - Max) :-
    Coverage = Name - List,
    maplist(coverage_term_length, List, Lengths),
    max_list(Lengths, Max).

is_beforefore(Coverage) :-
    Coverage = _-[_-[_-_|_]|_],
    not(Coverage = _-[_-[_-[_-_|_]|_]|_]).

max_list([], 0).
max_list(List, Max) :-
    sort(List, Sorted),
    append(_, [Max], Sorted).

coverage_term_length(Term - _, Length) :-
    term_string(Term, S),
    string_codes(S,C),
    length(C, Length).


table_dimensions(Coverage, Dimensions) :-
    row_counts(ver, Coverage, Dimensions).
row_counts(Direction,_ - Structure, dim(Direction, Len, Counts)) :-
    swap(Direction,New_Dir),
    not_coverage(Structure),
    !,
    length(Structure, Len),
    maplist(row_counts(New_Dir), Structure, Counts).
row_counts(_, _ - Coverage, Len) :-
    length(Coverage, Len).

swap(ver, hor).
swap(hor, ver).

not_coverage([_ - List | _]) :- is_list(List).

process(dim(ver, _Len, Tree), [[Header, nl] | Table], Header - Structure, Length_info) :-
    launch_process(process, Tree, Table_1, Structure, Length_info),
    append(Table_1, Table_2),
    maplist(append([pad(3)]), Table_2, Table).

process(dim(hor, _Len, Tree), [[Header, nl] | Table], Header - Structure, Length_info) :-
    launch_process(process, Tree, Table_1, Structure, Length_info),
    
    maplist(add_hor_structure, Length_info, Table_1, Table_2),
    
    pad(Table_2, Table_4),

    maplist(substitute_nones, Table_4, Table_5),
    utils:transpose(Table_5, Table_6),

    maplist(append, Table_6, Table_7),
    add_ver_structure(Table_7, Table).

process(_, Result, Header - Structure, _) :-
    process_coverage_col(Header - Structure, Result).

process_coverage_col(Header - Structure, [[Header, pad(4), coverage]| Table]) :-
    utils:rearrange(Structure,
        K - C,
        [' ', K, pad(5), size(7), C],
        Table).

pop([E | T], E, T) :- !.
pop([], none, []).
    
pad(Empty_rows, Empty_rows) :- maplist('='([]), Empty_rows), !.
pad(Rows, Result) :-
    maplist(pop, Rows, Elems, Remaining),
    pad(Remaining, Next),
    maplist(pop, Result, Elems, Next).

substitute_nones(List, Result) :-
    get_skeleton(List, Skeleton),
    maplist(substitute_none(Skeleton), List, Result).
    
get_skeleton(List, Skeleton) :-
    exclude(=(none), List, [_,E | _]),
    include(is_operator, E, Skeleton).
get_skeleton(_,[size(6), pad(5), size(7), pad(8)]).

substitute_none(Skeleton, none, Skeleton).
substitute_none(_, E, E):- not(E=none).

add_ver_structure(L, O) :-
    maplist(append([pad(3)]), L, L_1),
    add_to_end(nl, L_1, O).
add_hor_structure(_-Len, L, O) :-
    Size is max(Len, 6),
    maplist(append([size(Size)]), L, L_1),
    add_to_end(pad(8), L_1, O).

copy_with_elements(E, L, R) :-
    length(L, S),
    length(R, S),
    maplist(=(E), R).

add_to_end(E, L, R) :-
    copy_with_elements([E], L, El),
    maplist(append, L, El, R).

launch_process(Process, [D_1 | Dt_1], [Fill | Ft], [D_2 | Dt_2], Length_info) :-
    call(Process, D_1, Fill, D_2, Length_info),
    launch_process(Process, Dt_1, Ft, Dt_2, Length_info).
launch_process(_, [], [], [], _).


process_format([], '', []).

process_format([size(N)|Rest], Format, Args) :-
    absorb_atoms(Rest, Atoms, Rest_Next_Operator, Amount),
    process_format(Rest_Next_Operator, FRest, Next_Args),
    append(Atoms, Next_Args, Args),
    length(Atom_flags_l, Amount),
    maplist(=('~w'), Atom_flags_l),
    atomic_list_concat(Atom_flags_l, Atom_flags),
    atom_concat(Atom_flags, '~', Temp_1),
    atom_concat(N, '+', Temp_2),
    atom_concat(Temp_1, Temp_2, Sized),
    atom_concat(Sized, FRest, Format).

process_format([pad(N)|Rest], Format, Args) :-
    process_format(Rest, FRest, Args),
    length(SpacesList, N),
    maplist(=(' '), SpacesList),
    atom_chars(Spaces, SpacesList),
    atom_concat(Spaces, '~|', Padded),
    atom_concat(Padded, FRest, Format).

process_format([nl|Rest], Format, Args) :-
    process_format(Rest, FRest, Args),
    atom_concat('~n', FRest, Format).

process_format([Term|Rest], Format, [Term|ArgsRest]) :-
    not_operator(Term),
    process_format(Rest, FRest, ArgsRest),
    atom_concat('~w', FRest, Format).

absorb_atoms([Atom | List], [Atom | Atoms], List_remaining, Amount) :-
    not_operator(Atom),
    !,
    absorb_atoms(List, Atoms, List_remaining, Amount_inc),
    Amount is Amount_inc + 1.
absorb_atoms(LR, [], LR, 0).

not_operator(Atom) :- \+ member(Atom, [pad(_), size(_), nl]).
is_operator(Atom) :- member(Atom, [pad(_), size(_), nl]).