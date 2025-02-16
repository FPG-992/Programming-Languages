/*
** The configuations are represented using terms of the following form:
**   config(Man, Wolf, Goat, Cabbage)
** where each of the positions are atoms from the set {w,e}.
*/

/* goat_puzzle(?Moves) */
goat_puzzle(Moves) :- initial(C), length(Moves, _), solve(C, Moves).

initial(config(w, w, w, w)).
final(config(e, e, e, e)).

/* solve(+Configuration, -MoveList) */
solve(Config, []) :- final(Config).
solve(Config, [Move|Moves]) :-
   move(Config, Move, NextConfig),
   safe(NextConfig),
   solve(NextConfig, Moves).

move(config(A,A,G,C), wolf, config(B,B,G,C)) :- opposite(A, B).
move(config(A,W,A,C), goat, config(B,W,B,C)) :- opposite(A, B).
move(config(A,W,G,A), cabbage, config(B,W,G,B)) :- opposite(A, B).
move(config(A,W,G,C), nothing, config(B,W,G,C)) :- opposite(A, B).

opposite(e, w).
opposite(w, e).

safe(config(M,W,G,C)) :- together_or_opposite(M,W,G), together_or_opposite(M,G,C).

together_or_opposite(X,X,X).
together_or_opposite(_,X,Y) :- opposite(X,Y).