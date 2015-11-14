module(bitwise, [], []).

clausedef(test_shift_left, [], [int, int, int]).
test_shift_left(A, B, C) :-
    A is B << C.

clausedef(test_shift_right, [], [int, int, int]).
test_shift_right(A, B, C) :-
    A is B >> C.

clausedef(test_bitwise_and, [], [int, int, int]).
test_bitwise_and(A, B, C) :-
    A is B /\ C.

clausedef(test_bitwise_or, [], [int, int, int]).
test_bitwise_or(A, B, C) :-
    A is B \/ C.

