module(ill_typed1, [], []).

datadef(bool, [], [true, false]).

clausedef(test, [], []).
test :-
        Test = lambda([F, Ret],
                      (call(F, true, FP),
                       call(F, 0, P),
                       call(FP, P, Ret))).
