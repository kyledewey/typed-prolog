module(ground, [], []).

clausedef(test, [A], [A]).
test(A) :-
    ground(A).
