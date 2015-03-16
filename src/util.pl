:- module('util', [memberEqual/2, diffEqual/3, makeDistinctEqual/2,
                   bodyAtomForm/1, bodyPairForm/3, bodyPairForm/4,
                   addSet/3, setUnion/3, memberEqual_/2]).

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
