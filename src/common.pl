module(common, [map/3, filter/3, foldLeft/4, forall/2, exists/2,
                setContains/2, flatMap/3, foldRight/4, existsOnce/2,
                zip/3, find/3, beginsWith/2, contains/2,
                atomContains/2, notMember/2, appendDiffList/3,
                makeSetFromList/2, setUnion/3, setDifference/3,
                sortItems/4, onFailure/2, yolo_UNSAFE_format_shim/2,
                duplicates/2, setsOverlap/2, once/1],
                [pair, tup3, tup4, tup5, tup6, tup7, tup8, option]).

datadef(pair, [A, B], [pair(A, B)]).
datadef(tup3, [A, B, C], [tup3(A, B, C)]).
datadef(tup4, [A, B, C, D], [tup4(A, B, C, D)]).
datadef(tup5, [A, B, C, D, E], [tup5(A, B, C, D, E)]).
datadef(tup6, [A, B, C, D, E, F], [tup6(A, B, C, D, E, F)]).
datadef(tup7, [A, B, C, D, E, F, G], [tup7(A, B, C, D, E, F, G)]).
datadef(tup8, [A, B, C, D, E, F, G, H], [tup8(A, B, C, D, E, F, G, H)]).

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

clausedef(exists, [A], [list(A), relation([A])]).
exists(List, Relation) :-
    find(List, Relation, some(_)).

clausedef(existsOnce, [A], [list(A), relation([A])]).
existsOnce(List, Relation) :-
    findOnce(List, Relation, some(_)).

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

clausedef(setsOverlap, [A], [list(A), list(A)]).
setsOverlap(Set1, Set2) :-
    existsOnce(Set1,
               lambda([Item],
                      setContains(Set2, Item))).

clausedef(find, [A], [list(A), relation([A]), option(A)]).
find([], _, none).
find([H|_], Relation, some(H)) :-
        call(Relation, H).
find([_|T], Relation, Result) :-
        find(T, Relation, Result).

clausedef(findOnce, [A], [list(A), relation([A]), option(A)]).
findOnce(List, Relation, Result) :-
    once(lambda([], find(List, Relation, Result))).

clausedef(once, [], [relation([])]).
once(Relation) :-
    call(Relation),
    !.

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

clausedef(appendDiffList, [A], [list(A), list(A), list(A)]).
appendDiffList([], List, List).
appendDiffList([H|T], [H|Rest], Output) :-
        appendDiffList(T, Rest, Output).

clausedef(makeSetFromList, [A], [list(A), list(A)]).
makeSetFromList(List, Set) :-
        foldLeft(List, [],
                 lambda([Accum, CurElement, NewAccum],
                        (setContains(Accum, CurElement) ->
                            (Accum = NewAccum);
                            (NewAccum = [CurElement|Accum]))),
                 Set).

% uses == for comparison
clausedef(setUnion, [A], [list(A), list(A), list(A)]).
setUnion(Set1, Set2, FinalSet) :-
        append(Set1, Set2, List),
        makeSetFromList(List, FinalSet).

clausedef(setDifference, [A], [list(A), list(A), list(A)]).
setDifference(SetSource, SetMinus, FinalSet) :-
        foldLeft(SetSource, [],
                 lambda([Accum, Cur, NewAccum],
                        (setContains(SetMinus, Cur) ->
                            (Accum = NewAccum);
                            (NewAccum = [Cur|Accum]))),
                 FinalSet).

% inserts an item into a sorted list
clausedef(insertItem, [A], [list(A), relation([A, A]), A, list(A)]).
insertItem([], _, A, [A]) :- !.
insertItem([H|T], Comparer, A, [H|Rest]) :-
        call(Comparer, A, H),
        insertItem(T, Comparer, A, Rest),
        !.
insertItem([H|T], _, A, [A, H|T]).

clausedef(sortItems, [A, B], [list(A), % items to sort
                              relation([A, B]), % map them to this domain first
                              relation([B, B]), % compare in the other domain
                              list(A)]). % sorted according to B
sortItems(Items, Mapper, Comparer, SortedItems) :-
        map(Items,
            lambda([A, pair(A, B)], call(Mapper, A, B)),
            ItemPairs),
        PairComparer = lambda([pair(_, B1), pair(_, B2)], call(Comparer, B1, B2)),
        foldLeft(ItemPairs, [],
                 lambda([Accum, Pair, NewAccum],
                        insertItem(Accum, PairComparer, Pair, NewAccum)),
                 SortedPairs),
        map(SortedPairs,
            lambda([pair(A, _), A], true),
            SortedItems).

% If rel1 fails, then rel2 is called.  If rel1 succeeds, no backtracking will
% occur beyond this point, so rel2 will never be called.  If rel2 is called, failure
% will be propagated upwards.
clausedef(onFailure, [], [relation([]), relation([])]).
onFailure(Rel1, _) :-
        call(Rel1),
        !.
onFailure(_, Rel2) :-
        call(Rel2),
        !,
        fail.

clausedef(yolo_UNSAFE_format_shim, [A], [atom, list(A)]).
yolo_UNSAFE_format_shim(Atom, List) :-
        format(Atom, List).

% gets the elements that are duplicates, using == for comparison
clausedef(duplicates, [A], [list(A), list(A)]).
duplicates(Items, Duplicates) :-
        foldLeft(Items, pair([], []),
                 lambda([pair(Seen, Duplicated),
                         Item,
                         pair(NewSeen, NewDuplicated)],
                        ((setContains(Seen, Item) -> 
                            ((setContains(Duplicated, Item) -> 
                                (NewDuplicated = Duplicated);
                                (NewDuplicated = [Item|Duplicated])),
                             NewSeen = Seen);
                            (NewSeen = [Item|Seen],
                             NewDuplicated = Duplicated)))),
                 pair(_, Duplicates)).
