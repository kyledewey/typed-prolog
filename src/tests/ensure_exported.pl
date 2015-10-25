module(ensure_exported, [bar/1, bar/2], []).

% Should not compile, as bar/1 doesn't exist.
clausedef(bar, [], [atom, atom]).
bar(_, _) :- true.
bar(_, _) :- false.
