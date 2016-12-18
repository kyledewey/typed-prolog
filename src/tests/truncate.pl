module(truncate, [], []).

clausedef(test, [], []).
test :-
    Y is truncate(0^(-2)).
