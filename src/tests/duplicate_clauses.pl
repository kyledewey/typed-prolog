module(duplicate_clauses, [], []).

clausedef(test1, [], [int, int]).
clausedef(test1, [], [int, int]).
test1(X, Y) :-
        X is Y.
