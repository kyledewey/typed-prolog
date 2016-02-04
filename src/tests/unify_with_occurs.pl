module(unify_with_occurs, [], []).

clausedef(test, [], []).
test :-
    unify_with_occurs_check(X, 7).
