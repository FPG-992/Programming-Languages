% Load the input file and parse the grid
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

% Define valid moves
move(dx(-1), dy(0)).   % W
move(dx(1), dy(0)).    % E
move(dx(0), dy(-1)).   % N
move(dx(0), dy(1)).    % S
move(dx(-1), dy(-1)).  % NW
move(dx(-1), dy(1)).   % SW
move(dx(1), dy(-1)).   % NE
move(dx(1), dy(1)).    % SE

% Check if the move is within bounds
within_bounds(N, X, Y) :-
    X >= 0, X < N,
    Y >= 0, Y < N.

% Find path from the current position to the target
find_path(Grid, X, Y, N, Path, Moves) :-
    length(Grid, N),
    target(X, Y, N),
    reverse(Path, Moves).

find_path(Grid, X, Y, N, Path, Moves) :-
    move(dx(Dx), dy(Dy)),
    X1 is X + Dx,
    Y1 is Y + Dy,
    within_bounds(N, X1, Y1),
    nth0(Y, Grid, Row),
    nth0(X, Row, CurrCars),
    nth0(Y1, Grid, NextRow),
    nth0(X1, NextRow, NextCars),
    NextCars < CurrCars,
    \+ member((X1, Y1), Path),
    find_path(Grid, X1, Y1, N, [(X1, Y1)|Path], Moves).

% Check if the current position is the target
target(X, Y, N) :-
    X is N - 1,
    Y is N - 1.

% Main predicate to find the moves
moves(FileName, Moves) :-
    load_grid(FileName, Grid),
    length(Grid, N),
    find_path(Grid, 0, 0, N, [(0, 0)], Moves), !.

% Handle the case where no path is found
moves(_, []).
