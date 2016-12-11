module(atom_number, [], []).

clausedef(test, [], []).
test :-
    atom_number(_, 123).
