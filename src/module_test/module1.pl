module(module1, [mylength/2], [mylist]).

datadef(mylist, [A], [mycons(A, mylist(A)), mynil]).

clausedef(mylength, [A], [mylist(A), int]).
mylength(mynil, 0).
mylength(mycons(_, Rest), Len) :-
        mylength(Rest, RestLen),
        Len is RestLen + 1.
