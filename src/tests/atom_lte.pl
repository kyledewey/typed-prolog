module(atom_lte, [], []).

clausedef(mkAtom, [], [atom]).
mkAtom(Atom) :- Atom = '<='.
mkAtom(Atom) :- Atom = 'div'.
