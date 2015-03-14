datadef(list, [A], [cons(A, list(A)), nil]).

clausedef(map, [A, B], [list(A), relation(A, B), list(B)]).
clausedef(add, [], [int, int, int]).

map(nil, _, nil).
map(cons(HA, TA), F, cons(HB, TB)) :-
        call(F, HA, HB),
        map(TA, F, TB).

