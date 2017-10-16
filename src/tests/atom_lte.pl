module(atom_lte, [], []).

% test at the end of the clause
clausedef(mkAtom, [], [atom]).
mkAtom(Atom) :- Atom = '<='.
mkAtom(Atom) :- Atom = 'div'.

% test within conjunction
clausedef(mkAtom2, [], [atom]).
mkAtom2(Atom) :- Atom = '<=', Atom = '<='.

% test within disjunction
clausedef(mkAtom3, [], [atom]).
mkAtom3(Atom) :- Atom = '<='; Atom = 'div'.
