load_grid(FileName, Grid) :-
    open(FileName, read, Stream),
    read_line_to_codes(Stream, Line1),
    atom_codes(Atom1, Line1),
    atom_number(Atom1, N),
    read_grid(Stream, N, Grid),
    close(Stream).

read_grid(Stream, 0, []) :- !.
read_grid(Stream, N, [Row|Grid]) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, Row),
    N1 is N - 1,
    read_grid(Stream, N1, Grid).

move(dx(-1), dy(0), w).   % W
move(dx(1), dy(0), e).    % E
move(dx(0), dy(-1), n).   % N
move(dx(0), dy(1), s).    % S
move(dx(-1), dy(-1), nw). % NW
move(dx(-1), dy(1), sw).  % SW
move(dx(1), dy(-1), ne).  % NE
move(dx(1), dy(1), se).   % SE

within_bounds(N, X, Y) :-
    X >= 0, X < N,
    Y >= 0, Y < N.

bfs(Grid, N, Path) :-
    bfs([[0, 0, []]], Grid, N, [(0, 0)], Path).

bfs([[X, Y, Moves]|_], _, N, _, Moves) :-
    target(X, Y, N).

bfs([[X, Y, Moves]|Queue], Grid, N, Visited, Path) :-
    findall([X1, Y1, [Dir|Moves]],
            (move(dx(Dx), dy(Dy), Dir),
             X1 is X + Dx, Y1 is Y + Dy,
             within_bounds(N, X1, Y1),
             nth0(Y, Grid, Row), nth0(X, Row, Cars),
             nth0(Y1, Grid, NextRow), nth0(X1, NextRow, NextCars),
             NextCars < Cars, \+ member((X1, Y1), Visited)),
            NextMoves),
    append(Queue, NextMoves, NewQueue),
    append(Visited, [[X, Y]], NewVisited),
    bfs(NewQueue, Grid, N, NewVisited, Path).

target(X, Y, N) :-
    X is N - 1,
    Y is N - 1.

moves(FileName, Moves) :-
    load_grid(FileName, Grid),
    length(Grid, N),
    (   bfs(Grid, N, Path)
    ->  reverse(Path, Moves)
    ;   Moves = 'IMPOSSIBLE'
    ).
