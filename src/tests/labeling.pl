module(labeling, [], []).

clausedef(test, [], [int]).
test(X) :-
        X #>= 0,
        X #=< 10,
        fd_labeling([X]).

