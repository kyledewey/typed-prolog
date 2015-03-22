module(nested_capture, [], []).

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

clausedef(outer, [], [int]).
outer(Int) :-
        onFailure(
            lambda([], fail),
            lambda([],
                   (yolo_UNSAFE_format_shim('OUTER FAILURE OCCURRED~n', []),
                    onFailure(lambda([],
                                     (yolo_UNSAFE_format_shim('INTEGER: ~w~n', [Int]))),
                              lambda([],
                                     (yolo_UNSAFE_format_shim('UNREACHABLE~n', []))))))).
