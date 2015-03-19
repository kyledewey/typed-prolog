:- module('util', [memberEqual/2, diffEqual/3, makeDistinctEqual/2,
                   bodyAtomForm/1, bodyPairForm/3, bodyPairForm/4,
                   addSet/3, setUnion/3, memberEqual_/2, sortItems/4,
                   bodyVarForm/3, bodyVarForm/4, appendDiffList/3]).

% -Probe: A
% -List:  [A]
%
% like member, except it uses == instead of =
memberEqual(A, [H|T]) :-
        A == H; memberEqual(A, T).

% -List:  [A]
% -Probe: A
memberEqual_(List, Probe) :-
        memberEqual(Probe, List).

% -OriginalSet: [A]
% -Item: A
% -NewSet: [A]
%
% Uses == instead of =
addSet(Original, Item, [Item|Original]) :-
        \+ memberEqual(Item, Original),
        !.
addSet(Original, _, Original).

% -Set1: [A]
% -Set2: [A]
% -Result: [A]
%
% Uses == instead of =
setUnion([], Set2, Set2).
setUnion([H|T], Set2, Result) :-
        addSet(Set2, H, TempSet),
        setUnion(T, TempSet, Result).

% -Input: [A]
% -Item:  A
% -New:   [A]
setRemove(Input, Item, New) :-
        diffEqual(Input, [Item], New).

% -Input:         [A]
% -SubtractThese: [A]
% -Output:        [A]
%
% Set difference.  Uses equality to avoid unification.
diffEqual([], _, []).
diffEqual([H|T], SubtractThese, Output) :-
        (memberEqual(H, SubtractThese) ->
            Output = Rest;
            Output = [H|Rest]),
        diffEqual(T, SubtractThese, Rest).

% -List: [A]
% -Set:  [A]
makeDistinctEqual(List, Set) :-
        makeDistinctEqual(List, [], Set).

% -List: [A]
% -Seen: [A]
% -Set:  [A]
makeDistinctEqual([], _, []).
makeDistinctEqual([H|T], Seen, Result) :-
        (memberEqual(H, Seen) ->
            (NewSeen = Seen,
             Result = Rest);
            (NewSeen = [H|Seen],
             Result = [H|Rest])),
        makeDistinctEqual(T, NewSeen, Rest).

% -InputBody: Body
%
% Succeeds if it's an atom form, like true or false
bodyAtomForm(InputBody) :-
        atom(InputBody),
        member(InputBody, [true, fail, false]).

% -InputBody: Body
% -Name:      Name
% -B1:        Body
% -B2:        Body
%
% Tests if the input is a "pair form", like conjunction and disjunction.
% If so, it will unify `B1` and `B2` with the members of the pair.  This
% is here to avoid lots of repitition; these forms behave similarly across
% the board.
bodyPairForm(InputBody, Name, B1, B2) :-
        InputBody =.. [Name, B1, B2],
        member(Name, [',', ';', '->']).

% -InputBody: Body
% -B1:        Body
% -B2:        Body
%
% Like `bodyPairForm/4`, but it ignores the name.
bodyPairForm(InputBody, B1, B2) :-
        bodyPairForm(InputBody, _, B1, B2).

% -InputBody: Body
% -VarName:   Name, an atom
% -Term:      Term
bodyVarForm(InputBody, VarName, Term) :-
        bodyVarForm(InputBody, _, VarName, Term).

% -InputBody: Body
% -VarOpName: setvar | getvar
% -VarName:   Name, an atom
% -Term:      Term
bodyVarForm(InputBody, VarOpName, VarName, Term) :-
        InputBody =.. [VarOpName, VarName, Term],
        member(VarOpName, ['setvar', 'getvar']).

% -MapRelation: (A, B)
% -Item:        A
% -Pair:        pair(A, B)
mapPair(MapRelation, A, pair(A, B)) :-
        call(MapRelation, A, B).

% -Items:       [pair(A, B)]
% -CmpRelation: (B, B)
% -Pair:        pair(A, B)
% -Inserted:    [pair(A, B)]
%
% Ignores A.
insertPair([], _, Pair, [Pair]) :- !.
insertPair([HeadPair|Rest], CmpRelation, InsertPair, [InsertPair, HeadPair|Rest]) :-
        HeadPair = pair(_, HeadB),
        InsertPair = pair(_, InsertB),
        \+ call(CmpRelation, HeadB, InsertB),
        !.
insertPair([HeadPair|Rest], CmpRelation, InsertPair, [HeadPair|RestResult]) :-
        insertPair(Rest, CmpRelation, InsertPair, RestResult).

% -Items:       [pair(A, B)]
% -CmpRelation: (B, B)
% -Accum:       [pair(A, B)]
% -Retval:      [pair(A, B)]
%
% Ignores A.
sortPairs([], _, Accum, Accum).
sortPairs([H|T], CmpRelation, Accum, Retval) :-
        insertPair(Accum, CmpRelation, H, NewAccum),
        sortPairs(T, CmpRelation, NewAccum, Retval).

% -Items:       [pair(A, B)]
% -CmpRelation: (B, B)
% -Sorted:      [pair(A, B)]
%
% Ignores A.
sortPairs(Items, CmpRelation, Sorted) :-
        sortPairs(Items, CmpRelation, [], Sorted).

% -Pair:  pair(A, B)
% -First: A
fst(pair(A, _), A).

% -Items:       [A]
% -MapRelation: (A, B)
% -CmpRelation: (B, B)
% -Sorted:      [A]
%
% Sorts the given items, calling the map relation on them first
% and then sorting by the CmpRelation.  The map relation is called
% only once for each item.
sortItems(Items, MapRelation, CmpRelation, Sorted) :-
        maplist(mapPair(MapRelation), Items, ItemPairs),
        sortPairs(ItemPairs, CmpRelation, SortedPairs),
        maplist(fst, SortedPairs, Sorted).

% -What:   [A]
% -Input:  [A]
% -Output: [A]
appendDiffList([], List, List).
appendDiffList([H|T], [H|Rest], Output) :-
        appendDiffList(T, Rest, Output).

