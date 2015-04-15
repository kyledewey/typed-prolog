module(not_equal, [], []).

clausedef(test, [A], [A, A]).
test(A, B) :-
        A \== B.