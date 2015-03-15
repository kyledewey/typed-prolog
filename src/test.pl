datadef(list, [A], [cons(A, list(A)), nil]).

clausedef(map, [A, B], [list(A), relation(A, B), list(B)]).

map(nil, _, nil).
map(cons(HA, TA), F, cons(HB, TB)) :-
        call(F, HA, HB),
        map(TA, F, TB).

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
        A = nil.

%% call_lambda0(lambda0_0) :-
%%         A is 1.

%% translatedTest :-
%%         X = lambda0_0,
%%         call_lambda0(X).
