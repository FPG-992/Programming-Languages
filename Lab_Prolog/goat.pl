/* c
** config(Man,Wolf,Goat,Cabbage). 
*/

 /* Thelume na kanoume mia anazhthsh se ena dendro katastasewn 
 **arithmos elaxistwn vhmatos, mia seira apo moves gia na ftasw sthn telikh katastash
 */

goat_puzzle(Moves) :- initial(C), length(Moves, _), solve(C, Moves).

initial(config(w,w,w,w)).
final(config(e,e,e,e)).

/* solve(+Configuration, -MoveList) */
solve(Config,[]) :- final(Config).
solve(Config, [Move|Moves]) :- 
safe(NextConfig),
move(Config,Move,NextConfig),
solve(NextConfig, Moves).

move(config(A,A,G,C), Wolf, config(B,B,G,C)) :- opposite(A,B).
move(config(A,W,A,C), Goat, config(B,W,B,C)) :- opposite(A,B).
move(config(A,W,G,A), Cabbage, config(B,W,G,B)) :- opposite(A,B).
move(config(A,W,G,C), Nothing, config(B,W,G,C)) :- opposite(A,B).


opposite(e,w).
opposite(w,e).

safe(Config(M,W,G,C)) :- together_or_opposite(M,W,G), together_or_seperated(M,G,C).

together_or_opposite(X,X,X).
together_or_opposite(_,X,Y) :- opposite(X,Y).