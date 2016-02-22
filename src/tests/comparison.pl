module(comparison, [], []).

clausedef(test1, [], []).
test1 :-
    3 + 7 < 10 + 11.

clausedef(test2, [], [int]).
test2(Y) :-
    X #= 3 * Y,
    X #> 7 + 8.

