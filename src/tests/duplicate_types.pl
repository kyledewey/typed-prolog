module(duplicate_types, [], []).

datadef(foo, [], [foo]).
datadef(foo, [], [bar]).

clausedef(useFoo, [], [foo]).
useFoo(foo).
