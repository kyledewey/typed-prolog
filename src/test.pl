datadef(list, [A], [cons(A, list(A)), nil]).

clausedef(map, [A, B], [list(A), relation(A, B), list(B)]).

map(nil, _, nil).
map(cons(HA, TA), F, cons(HB, TB)) :-
        call(F, HA, HB),
        map(TA, F, TB).

clausedef(add, [], [int, int, int]).
add(X, Y, Z) :-
        Z is X * Y.

