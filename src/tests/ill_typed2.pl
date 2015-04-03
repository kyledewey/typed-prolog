module(ill_typed2, [], []).

datadef(moo, [], [moo]).
datadef(cow, [], [cow]).

clausedef(foo, [], [int, moo, cow]).
foo(_, Moo, Cow) :-
        foo2(Moo, Cow).

clausedef(foo2, [], [cow, moo]).
foo2(cow, moo).
