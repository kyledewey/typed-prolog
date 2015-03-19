module(common, [map/3, filter/3, foldLeft/4, forall/2,
                setContains/2], [pair, option]).

datadef(pair, [A, B], [pair(A, B)]).
datadef(option, [A], [some(A), none]).

clausedef(map, [A, B], [list(A), relation([A, B]), list(B)]).
map([], _, []).
map([HA|TA], F, [HB|TB]) :-
        call(F, HA, HB),
        map(TA, F, TB).

clausedef(filter, [A], [list(A), relation([A]), list(A)]).
filter([], _, []).
filter([H|T], R, ResultList) :-
        (call(R, H) ->
            ResultList = [H|Rest];
            ResultList = Rest),
        filter(T, R, Rest).

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

% Compares using equality instead of unification
clausedef(setContains, [A], [list(A), A]).
setContains([H|_], Item) :-
        H == Item.
setContains([_|T], Item) :-
        setContains(T, Item).
