clausedef(map, [A, B], [list(A), relation([A, B]), list(B)]).

map([], _, []).
map([HA|TA], F, [HB|TB]) :-
        call(F, HA, HB),
        map(TA, F, TB).

clausedef(filter, [A], [list(A), relation([A]), list(A)]).
filter([], _, []).
filter([H|T], R, ResultList) :-
        (call(R, H) ->
            ResultList = [H|Rest];
            ResultList = Rest),
        filter(T, R, Rest).

clausedef(lessThanN, [], [list(int), int, list(int)]).
lessThanN(List, N, NewList) :-
        filter(List, lambda([Cur], Cur < N), NewList).

clausedef(foldLeft, [A, B], [list(A), B, relation([B, A, B]), B]).
foldLeft([], Accum, _, Accum).
foldLeft([H|T], Accum, Relation, Result) :-
        call(Relation, Accum, H, NewAccum),
        foldLeft(T, NewAccum, Relation, Result).

clausedef(addListWithFoldLeft, [], [list(int), int]).
addListWithFoldLeft(List, Retval) :-
        foldLeft(List, 0, lambda([Acc, N, NewAcc], NewAcc is N + Acc), Retval).

clausedef(addList, [], [list(int), int]).
addList(List, Retval) :-
        Helper = lambda([CurList, Accum],
            (CurList = [H|T] ->
                (NewAccum is Accum + H,
                 call(Helper, T, NewAccum));
                (Retval = Accum))),
        call(Helper, List, 0),
        ensureType(Helper).

clausedef(ensureType, [], [relation([list(int), int])]).
ensureType(_).

clausedef(add, [], [int, int, int]).
add(X, Y, Z) :-
        Z is X * Y.

clausedef(plus1, [], [list(int), list(int)]).
plus1(Input, Output) :-
        map(Input, lambda([X, Y], Y is X + 1), Output).

clausedef(test, [], []).
test :-
        X = lambda([], A is 1),
        call(X),
        A = [].

clausedef(compare, [], [int, int]).
compare(X, Y) :-
        X =< Y.

clausedef(runTests, [], []).
runTests :-
        plus1([1,2,3], Res1),
        Res1 == [2,3,4],

        lessThanN([1,2,3,4,5], 3, Res2),
        Res2 == [1,2],

        addListWithFoldLeft([1,2,3], Res3),
        Res3 == 6,

        addList([4,5,6], Res4),
        Res4 == 15,

        test.
