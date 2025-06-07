:- use_module(library(readutil)).
:- use_module(library(assoc)).

crossword(File) :-
    setup_call_cleanup(
        open(File, read, S),
        ( read_problem(S, R, C, Blacks, Words),
          build_grid(R, C, Grid),
          mark_blacks(Blacks, Grid),
          slots(Grid, SlotList0),
          sort_slots(SlotList0, Slots),
          bucket_by_len(Words, Dict0),
          once(label_slots(Slots, Dict0)),
          maplist(print_row, Grid)
        ),
        close(S)
    ).

read_problem(S, R, C, Blacks, Words) :-
    read_line_to_codes(S, L1),
    phrase(numbers([R,C,B,W]), L1),
    read_blacks(B, S, Blacks),
    read_words(W, S, Words).

numbers([N|Ns]) --> blanks, int(N), !, numbers(Ns).
numbers([]) --> [].
int(N) --> digits(Ds), { number_codes(N, Ds) }.
digits([D|Ds]) --> digit(D), !, digits(Ds).
digits([]) --> [].
digit(D) --> [D], { between(0'0,0'9,D) }.
blanks --> [C], { char_type(C, space) }, !, blanks.
blanks --> [].

read_blacks(0, _, []) :- !.
read_blacks(N, S, [(R,C)|Bs]) :-
    N > 0,
    read_line_to_codes(S, L),
    L \== end_of_file,
    phrase(numbers([R,C]), L),
    N1 is N-1,
    read_blacks(N1, S, Bs).

read_words(0, _, []) :- !.
read_words(N, S, [W|Ws]) :-
    N > 0,
    read_line_to_codes(S, Codes0),
    Codes0 \== end_of_file,
    exclude(=(13), Codes0, Codes),
    Codes \== [],
    string_codes(Str, Codes),
    atom_string(W, Str),
    N1 is N-1,
    read_words(N1, S, Ws).

build_grid(R,C,G) :- length(G,R), maplist(make_row(C), G).
make_row(C, Row) :- length(Row,C).

mark_blacks([], _).
mark_blacks([(R,C)|Bs], G) :- nth1(R,G,Row), nth1(C,Row,#), mark_blacks(Bs,G).

slots(G, Slots) :-
    across_slots(G, AS),
    transpose(G, GT),
    across_slots(GT, DS),
    append(AS, DS, Slots).

across_slots([], []).
across_slots([Row|Rs], Slots) :-
    row_slots(Row, RowSlots),
    across_slots(Rs, Rest),
    append(RowSlots, Rest, Slots).

row_slots(Row, Slots) :-
    append(_,Tail,Row),
    Tail \== [],
    Tail = [H|_],
    H \== #,
    !,
    prefix_unhash(Tail, Slot, After),
    (   length(Slot,L), L>=2 -> Slots=[Slot|S1] ; Slots=S1 ),
    row_slots(After, S1).
row_slots(_, []).

prefix_unhash([C|T],[C|S],Rest) :- C \== #, !, prefix_unhash(T,S,Rest).
prefix_unhash(Rest, [], Rest).

transpose([], []).
transpose([F|Fs], Ts) :- transpose_col(F,[F|Fs],Ts).
transpose_col([],_,[]).
transpose_col([_|_],Rows,[Col|Cols]) :- maplist(list_first_rest,Rows,Col,Tails), transpose(Tails,Cols).
list_first_rest([H|T],H,T).

sort_slots(Slots, Ordered) :-
    map_list_to_pairs(length, Slots, Pairs),
    keysort(Pairs, Rev),
    reverse(Rev, BigFirst),
    pairs_values(BigFirst, Ordered).

bucket_by_len(Ws, Dict) :- empty_assoc(E), foldl(add_word, Ws, E, Dict).
add_word(W, In, Out) :-
    atom_length(W,L),
    ( get_assoc(L,In,Ls) -> put_assoc(L,In,[W|Ls],Out)
    ; put_assoc(L,In,[W],Out) ).

label_slots([], _).
label_slots([S|Ss], Dict0) :-
    length(S,L),
    get_assoc(L, Dict0, Bucket),
    select(W, Bucket, Rest),
    atom_chars(W, Chars),
    S = Chars,
    put_assoc(L, Dict0, Rest, Dict),
    label_slots(Ss, Dict).

print_row(Row) :-
    split_on_hash(Row, Segs),
    include(len_ge2, Segs, Good),
    maplist(atom_chars, Words, Good),
    atomic_list_concat(Words, ' ', Line),
    writeln(Line).

split_on_hash([], [[]]).
split_on_hash([#|T],[[]|Segs]) :- split_on_hash(T,Segs).
split_on_hash([C|T],[[C|Cs]|Segs]) :- C \== #, split_on_hash(T,[Cs|Segs]).

len_ge2(L) :- length(L,N), N>=2.