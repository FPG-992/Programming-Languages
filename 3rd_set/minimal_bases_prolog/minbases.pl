minbases(Numbers, Bases) :-
    maplist(minbase, Numbers, Bases).

minbase(Number, Base) :-
    minbase(Number, 2, Base).

minbase(Number, Base, Base) :-
    to_base(Number, Base, Digits),
    all_same(Digits), !.
minbase(Number, CurrentBase, Base) :-
    NextBase is CurrentBase + 1,
    minbase(Number, NextBase, Base).

to_base(0, _, []) :- !.
to_base(Number, Base, [Digit|Digits]) :-
    Number > 0,
    Digit is Number mod Base,
    NextNumber is Number // Base,
    to_base(NextNumber, Base, Digits).

all_same([]).
all_same([_]).
all_same([X, X | T]) :-
    all_same([X | T]).


