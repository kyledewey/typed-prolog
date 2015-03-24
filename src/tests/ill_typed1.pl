module(ill_typed1, [], []).

datadef(bool, [], [true, false]).

clausedef(test, [], []).
test :-
        Test = lambda([F],
                      (call(F, true),
                       call(F, 0))).
