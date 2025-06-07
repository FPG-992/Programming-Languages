:- module(boxes, [boxes/2]).
:- use_module(library(readutil)).
:- use_module(library(apply)).

boxes(File, Comb) :-
    setup_call_cleanup(
        open(File, read, S),
        (
            read_line_to_string(S, L1),
            split_string(L1, " ", "", [_N,Tstr]),
            number_string(Target, Tstr),
            read_line_to_string(S, L2),
            split_string(L2, " ", "", SStrs),
            maplist(number_string, Raw, SStrs),
            sort(Raw, Sizes),
            combo_sum(Sizes, Target, Comb, 0)
        ),
        close(S)
    ).

combo_sum(_, 0, [], _).
combo_sum(Sizes, Sum, [X|Rest], Min) :-
    member(X, Sizes),
    X >= Min,
    X =< Sum,
    Sum1 is Sum - X,
    combo_sum(Sizes, Sum1, Rest, X).