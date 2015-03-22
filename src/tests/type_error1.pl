module(type_error1, [], []).

datadef(foo, [], [foo]).
datadef(bar, [], [bar]).

clausedef(needsFoo, [], [foo]).
needsFoo(_).

clausedef(callsFoo, [], []).
callsFoo :-
        needsFoo(bar).
