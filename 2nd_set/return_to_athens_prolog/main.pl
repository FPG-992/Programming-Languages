:- use_module(library(lists)).
:- use_module(library(pio)).

read_and_return(File, N, Grid) :-
    open(File, read, Stream),
    read_line(Stream, [N]),
    read_grid(Stream, N, Grid),
    close(Stream).

read_grid(Stream, N, Grid) :-
    ( N > 0 ->
        Grid = [Row|Rows],
        read_line(Stream, Row),
        N1 is N - 1,
        read_grid(Stream, N1, Rows)
    ; N =:= 0 ->
        Grid = []
    ).

read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List).

moves(File, Moves) :-
    exists_file(File),
    read_and_return(File, N, Grid),
    format('Grid: ~w~n', [Grid]), % Debugging output
    bfs(Grid, N, Path),
    format('Raw Path: ~w~n', [Path]), % Debugging output
    moves_list(Path, Moves).

bfs(Grid, N, Path) :-
    bfs([[0-0]], Grid, N, [], ReversedPath),
    reverse(ReversedPath, Path).

bfs([[X-Y|Path]|_], _, N, _, [[X-Y|Path]]) :-
    X =:= N - 1,
    Y =:= N - 1.
bfs([Path|Paths], Grid, N, Visited, Result) :-
    Path = [X-Y|_],
    format('Visiting: ~w-~w~n', [X, Y]), % Debugging output
    findall([NX-NY,X-Y|Path],
            (move(X, Y, NX, NY, Grid, N),
             \+ memberchk(NX-NY, Visited)),
            NextPaths),
    append(Paths, NextPaths, NewPaths),
    bfs(NewPaths, Grid, N, [X-Y|Visited], Result).

move(X, Y, NX, NY, Grid, N) :-
    member(d(DX, DY, _Move), [d(-1, -1, nw), d(-1, 0, n), d(-1, 1, ne),
                             d(0, -1, w), d(0, 1, e), d(1, -1, sw),
                             d(1, 0, s), d(1, 1, se)]),
    NX is X + DX,
    NY is Y + DY,
    NX >= 0, NX < N, NY >= 0, NY < N,
    nth0(NX, Grid, Row), nth0(NY, Row, Cars),
    nth0(X, Grid, Row1), nth0(Y, Row1, Cars1),
    Cars < Cars1,
    format('Move from ~w-~w to ~w-~w with ~w cars (current: ~w)~n', [X, Y, NX, NY, Cars, Cars1]).

moves_list([], []).
moves_list([_], []).
moves_list([[X1, Y1], [X2, Y2] | Rest], [Move | Moves]) :-
    direction(X1, Y1, X2, Y2, Move),
    moves_list([[X2, Y2] | Rest], Moves).

direction(X1, Y1, X2, Y2, Move) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    memberchk(d(DX, DY, Move), [d(-1, -1, nw), d(-1, 0, n), d(-1, 1, ne),
                                d(0, -1, w), d(0, 1, e), d(1, -1, sw),
                                d(1, 0, s), d(1, 1, se)]).

lines([L|Ls]) --> line(L), lines(Ls).
lines([]) --> [].

line([C|Cs]) --> [C], { \+ char_type(C, newline) }, line(Cs).
line([]) --> [C], { char_type(C, newline) }.

split_at(0, L, [], L).
split_at(N, [H|T], [H|L], R) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, L, R).
