module(module3, [], []).

use_module('module1.pl', [mylength/2], [mylist]).
use_module('module2.pl', [myappend/3], []).

clausedef(module_tests, [], []).
module_tests :-
        mylength(mycons(0, mycons(1, mynil)), Res1),
        Res1 == 2,

        myappend(mycons(0, mycons(1, mynil)),
                 mycons(2, mycons(3, mynil)),
                 Res2),
        Res2 == mycons(0, mycons(1, mycons(2, mycons(3, mynil)))).
