module(more_binops, [], []).

clausedef(test, [], [int, int]).
test(X, Y) :-
    X is 2^Y,
    Y is X mod 2.
