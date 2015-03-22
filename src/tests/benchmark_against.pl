% -List[A]
% -B
% -relation(B, A, B)
% -B
foldLeft([], Accum, _, Accum).
foldLeft([H|T], Accum, Relation, Result) :-
        call(Relation, Accum, H, TempAccum),
        foldLeft(T, TempAccum, Relation, Result).

add(A, B, C) :-
        C is A + B.

addListWithFoldLeft(List, Retval) :-
        foldLeft(List, 0, add, Retval).

makeList(0, _, []).
makeList(N, Item, [Item|Rest]) :-
        NewN is N - 1,
        makeList(NewN, Item, Rest).

benchmarkBigList :-
        makeList(50000000, 1, List),
        addListWithFoldLeft(List, _).
