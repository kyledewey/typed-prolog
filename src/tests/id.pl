module(id, [], []).

datadef(bool, [], [true, false]).

clausedef(test, [], []).
test :-
        Id = lambda([X, X], true),
        call(Id, true, _),
        call(Id, 0, _).
