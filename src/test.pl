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

clausedef(foldLeft, [A, B], [list(A), B, relation([B, A, B]), B]).
foldLeft([], Accum, _, Accum).
foldLeft([H|T], Accum, Relation, Result) :-
        call(Relation, Accum, H, NewAccum),
        foldLeft(T, NewAccum, Relation, Result).

clausedef(addList, [], [list(int), int]).
addList(List, Retval) :-
        Helper = lambda([CurList, Accum],
            (CurList = [H|T] ->
                (NewAccum is Accum + H,
                 call(Helper, T, NewAccum));
                (Retval = Accum))),
        call(Helper, List, 0),
        ensureType(Helper).

%% translatedAddList :-
%%         Helper = lambda2_0(Retval, Helper),
%%         call_lambda2(Helper, List, 0).

%% call_lambda2(lambda2_0(Retval, Helper), CurList, Accum) :-
%%         (CurList = [H|T] ->
%%             (NewAccum is Accum + H,
%%              call_lambda2(Helper, T, NewAccum));
%%             (Retval = Accum)).

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

%% call_lambda0(lambda0_0) :-
%%         A is 1.

%% translatedTest :-
%%         X = lambda0_0,
%%         call_lambda0(X).
