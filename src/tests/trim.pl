module(trim, [], []).

clausedef(doNotTrim, [], []).
doNotTrim :-
        X = 1.

clausedef(trimMe, [], [relation([int])]).
trimMe(F) :-
        call(F, 7).
