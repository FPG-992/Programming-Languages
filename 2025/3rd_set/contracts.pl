:- use_module(library(readutil)).
:- use_module(library(heaps)).
:- use_module(library(lists)).

hire(File, Profit) :-
    open(File, read, S),
    read_line_to_string(S, L0),
    split_string(L0, " ", "", [M0,K0]),
    number_string(M, M0),
    number_string(K, K0),
    N is M+K,
    empty_heap(H),
    read_rows(S, N, M, SumB, H, Hf),
    heap_to_list(Hf, Pairs),
    pairs_keys(Pairs, Ks),
    sum_list(Ks, Extra),
    Profit is SumB+Extra,
    close(S), !.

read_rows(_, 0, _, 0, H, H).
read_rows(S, N, M, SumB, H0, Hf) :-
    read_line_to_string(S, L),
    split_string(L, " ", "", [A0,B0]),
    number_string(A, A0),
    number_string(B, B0),
    Diff is A-B,
    (Diff>0 ->
        add_to_heap(H0, Diff, x, H1),
        heap_size(H1, Sz),
        (Sz>M -> get_from_heap(H1, _, _, H2) ; H2=H1)
    ; H2=H0),
    N1 is N-1,
    read_rows(S, N1, M, SB, H2, Hf),
    SumB is SB+B.