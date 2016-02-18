module(export_foo, [fooProcedure/1], [foo]).

clausedef(fooProcedure, [], [int]).
fooProcedure(1).

datadef(foo, [], [foo]).
