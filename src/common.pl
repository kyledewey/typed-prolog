module(common, [map/3, filter/3, foldLeft/4, forall/2,
                setContains/2, flatMap/3, foldRight/4,
                zip/3, find/3, beginsWith/2, contains/2,
                atomContains/2, notMember/2],
                [pair, option]).

datadef(pair, [A, B], [pair(A, B)]).
datadef(option, [A], [some(A), none]).

clausedef(map, [A, B], [list(A), relation([A, B]), list(B)]).
map([], _, []).
map([HA|TA], F, [HB|TB]) :-
        call(F, HA, HB),
        map(TA, F, TB).

clausedef(flatMap, [A, B], [list(A), relation([A, list(B)]), list(B)]).
flatMap(List, Relation, ResultList) :-
        foldRight(
            List, [],
            lambda([Cur, Accum, NewAccum],
                (call(Relation, Cur, CurList),
                 append(CurList, Accum, NewAccum))),
            ResultList).

clausedef(filter, [A], [list(A), relation([A]), list(A)]).
filter([], _, []).
filter([H|T], R, ResultList) :-
        (call(R, H) ->
            ResultList = [H|Rest];
            ResultList = Rest),
        filter(T, R, Rest).

clausedef(foldRight, [A, B], [list(A), B, relation([A, B, B]), B]).
foldRight([], Accum, _, Accum).
foldRight([H|T], Accum, Relation, Result) :-
        foldRight(T, Accum, Relation, TailAccum),
        call(Relation, H, TailAccum, Result).

clausedef(foldLeft, [A, B], [list(A), B, relation([B, A, B]), B]).
foldLeft([], Accum, _, Accum).
foldLeft([H|T], Accum, Relation, Result) :-
        call(Relation, Accum, H, NewAccum),
        foldLeft(T, NewAccum, Relation, Result).

clausedef(forall, [A], [list(A), relation([A])]).
forall([], _).
forall([H|T], Relation) :-
        call(Relation, H),
        forall(T, Relation).

% unlike the usual definition, this will fail if the two input lists
% are not of the same length.
clausedef(zip, [A, B], [list(A), list(B), list(pair(A, B))]).
zip([], [], []).
zip([H1|T1], [H2|T2], [pair(H1, H2)|Rest]) :-
        zip(T1, T2, Rest).

% Compares using equality instead of unification
clausedef(setContains, [A], [list(A), A]).
setContains([H|_], Item) :-
        H == Item.
setContains([_|T], Item) :-
        setContains(T, Item).

clausedef(find, [A], [list(A), relation([A]), option(A)]).
find([], _, none).
find([H|_], Relation, some(H)) :-
        call(Relation, H).
find([_|T], Relation, Result) :-
        find(T, Relation, Result).

% compares using unification
clausedef(beginsWith, [A], [list(A), list(A)]).
beginsWith(_, []).
beginsWith([H|T1], [H|T2]) :-
        beginsWith(T1, T2).

clausedef(contains, [A], [list(A), list(A)]).
contains(List, Probe) :-
        beginsWith(List, Probe).
contains([_|T], Probe) :-
        contains(T, Probe).

clausedef(notMember, [A], [A, list(A)]).
notMember(A, List) :-
        forall(List, lambda([L], A \= L)).

clausedef(atomContains, [], [atom, atom]).
atomContains(Original, Probe) :-
        atom_codes(Original, OriginalList),
        atom_codes(Probe, ProbeList),
        contains(OriginalList, ProbeList).
