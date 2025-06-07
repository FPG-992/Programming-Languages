/*  ===============================================================
    crossword.pl   –   ECE-NTUA  PL-1   (June 2025)
    ===============================================================

      ?- [crossword].
      ?- crossword('input1.txt').

    ============================================================== */

:- module(crossword, [crossword/1]).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  1.  Entry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crossword(File) :-
    read_instance(File, R, C, Blacks, Words),
    format('Read: R=~w, C=~w, Blacks=~w~n', [R, C, Blacks]),
    format('Words: ~w~n', [Words]),
    build_grid(R, C, Blacks, Grid),
    format('Built grid:~n'),
    print_debug_grid(Grid),
    all_slots(Grid, Slots),
    length(Slots, NumSlots),
    format('Found ~w slots~n', [NumSlots]),
    (   NumSlots > 0 ->
        format('First few slots: ~w~n', [Slots])
    ;   true
    ),
    (   solve(Slots, Words)                       % search succeeds
    ->  format('Solution found!~n'),
        print_grid(Grid)
    ;   writeln('IMPOSSIBLE')
    ).

print_debug_grid([]).
print_debug_grid([Row|Rows]) :-
    format('Row: ~w~n', [Row]),
    print_debug_grid(Rows).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  2.  Read tokens - FIXED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_instance(File, R, C, Blacks, Words) :-
    read_file_to_codes(File, Cs, []),
    phrase(tokens(Ts), Cs),
    Ts = [R,C,NB,NW|Tail],
    take_pairs(NB, Tail, Blacks, AfterPairs),
    take_atoms(NW, AfterPairs, Words, []).

tokens([T|Ts])     --> blanks, token(T), !, tokens(Ts).
tokens([])         --> blanks, eos.
token(N)           --> integer(N).
token(A)           --> string_without(" \n\t\r", Cs),
                       {Cs\=[], atom_codes(A,Cs)}.

take_pairs(0,L,[],L) :- !.
take_pairs(N,[R,C|T],[(R,C)|Ps],Rest) :-
    N>0, N1 is N-1, take_pairs(N1,T,Ps,Rest).

take_atoms(0,L,[],L) :- !.
take_atoms(N,[W|T],[W|Ws],Rest) :-
    N>0, N1 is N-1, take_atoms(N1,T,Ws,Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  3.  Grid & slot enumeration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_grid(Rows, Cols, Blacks, Grid) :-
    length(Grid, Rows),
    maplist(length_list(Cols), Grid),
    maplist(mark_black(Grid), Blacks).

length_list(N, L) :- length(L, N).
mark_black(G, (R,C)) :- nth1(R,G,Row), nth1(C,Row,'#').

all_slots(Grid, Slots) :-
    h_slots(Grid,  H),
    transpose(Grid, T),
    h_slots(T, V),
    append(H, V, Slots).

h_slots([], []).
h_slots([Row|Rs], Slots) :-
    row_slots(Row, RowSlots),
    format('Row ~w produced slots: ~w~n', [Row, RowSlots]),
    h_slots(Rs, Rest),
    append(RowSlots, Rest, Slots).

row_slots(Row, Slots) :-
    row_scan(Row, [], [], Rev), reverse(Rev, Slots).

row_scan([], Curr, Acc, Out) :-          % flush tail
    ( length(Curr,L), L>=2 -> reverse(Curr,S), Out=[S|Acc]
    ; Out=Acc ).
row_scan([Cell|Rest], Curr, Acc, Out) :-
    ( Cell == '#' ->
        % Hit black square - finish current sequence
        ( length(Curr,L), L>=2 -> reverse(Curr,S), Acc1=[S|Acc]
        ; Acc1=Acc ),
        row_scan(Rest, [], Acc1, Out)
    ;   % Regular cell - add to current sequence
        row_scan(Rest, [Cell|Curr], Acc, Out)
    ).

transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).
firsts_rests([], [], []).
firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    firsts_rests(Rest, Fs, Oss).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  4.  Search  (fail-first, no zero-candidate slots) - FIXED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve([], []).                                   % done
solve(Slots, Words) :-
    select_best(Slots, Words, Slot, Cands),
    Cands \= [],                                 % ensure we have candidates
    member(Word, Cands),
    atom_chars(Word, Chars),
    Slot = Chars,                                % propagate letters
    select(Word, Words, Words1),
    delete(Slots, Slot, Slots1),
    solve(Slots1, Words1).

select_best(Slots, Words, Best, Cands) :-
    % keep only slots that still have at least one word
    include(slot_has_candidate(Words), Slots, Viable),
    Viable \= [],                                % fail if none
    maplist(slot_cand_pair(Words), Viable, Pairs),
    keysort(Pairs, [_-Best|_]),                  % fewest first
    findall(W, fits(Best, W, Words), Cands).

slot_has_candidate(Ws, Slot) :-
    findall(W, fits(Slot, W, Ws), Cands),
    Cands \= [].

slot_cand_pair(Words, Slot, N-Slot) :-
    findall(W, fits(Slot, W, Words), Cands),
    length(Cands, N).

% Check if a word fits in a slot
fits(Slot, Word, Words) :-
    member(Word, Words),
    atom_chars(Word, Chars),
    length(Slot, L1),
    length(Chars, L2),
    L1 =:= L2,
    fits_chars(Slot, Chars).

fits_chars([], []).
fits_chars([S|Ss], [C|Cs]) :-
    (var(S) -> true ; S = C),
    fits_chars(Ss, Cs).

atom_len(L, W) :- atom_length(W, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  5.  Printing - SIMPLIFIED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_grid([]).
print_grid([Row|Rs]) :-
    extract_horizontal_words(Row, Words),
    print_words_line(Words),
    print_grid(Rs).

% Extract words from a single row
extract_horizontal_words(Row, Words) :-
    extract_word_sequences(Row, [], [], RevWords),
    reverse(RevWords, Words).

extract_word_sequences([], Current, Acc, Result) :-
    finish_current_word(Current, Acc, Result).

extract_word_sequences(['#'|Rest], Current, Acc, Result) :- !,
    finish_current_word(Current, Acc, NewAcc),
    extract_word_sequences(Rest, [], NewAcc, Result).

extract_word_sequences([Char|Rest], Current, Acc, Result) :-
    Char \= '#',
    extract_word_sequences(Rest, [Char|Current], Acc, Result).

finish_current_word(Current, Acc, Result) :-
    (   Current = [] 
    ->  Result = Acc
    ;   length(Current, Len),
        (   Len >= 2
        ->  reverse(Current, WordChars),
            atom_chars(WordAtom, WordChars),
            Result = [WordAtom|Acc]
        ;   Result = Acc
        )
    ).

% Print words in a line
print_words_line([]) :- nl.
print_words_line([Word]) :- !,
    write(Word), nl.
print_words_line([Word|Words]) :-
    write(Word), write(' '),
    print_words_line(Words).