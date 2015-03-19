module(module2, [myappend/3], []).

use_module('module1.pl', [mylength/2], [mylist]).

clausedef(localLength, [A], [mylist(A), int]).
localLength(List, Len) :-
        mylength(List, Len).

clausedef(myappend, [A], [mylist(A), mylist(A), mylist(A)]).
myappend(mynil, List, List).
myappend(mycons(H, T), Other, mycons(H, Rest)) :-
        myappend(T, Other, Rest).
