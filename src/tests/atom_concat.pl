module(atom_concat, [], []).

clausedef(test, [], [atom]).
test(X) :-
    atom_concat('moo', 'cow', X).
