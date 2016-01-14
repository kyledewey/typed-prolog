# typed-prolog
A basic type system on top of Prolog, along with higher-order clauses.  Translates to normal Prolog.


## Central Features ##

Typed-Prolog provides the following basic features:

- An algebraic type system with local type inference, based on [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).
  This means the following two core components:
  - Generic types (introduced with datatype definitions)
  - Parametric polymorphism (only supported at the clause level)
- Higher-order clauses, the equivalent of higher-order functions in a logic programming setting.
  Type inference is automatic for these.
  These functions compile down to standard first-order Prolog, via [defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization).
  In practice, not only does this make such functions safer to use (we do not resort to any `eval`-like metaprogramming), these also run much faster.
- Staightforward modules
- Global variables

## Example ##

Below is an example which uses a variety of features of Typed-Prolog.

```prolog
module(routines, [myMap/3, sum/2, sumNative/2], [myList]).

datadef(myList, [A], [myCons(A, myList(A)), myNil]).

clausedef(foldRight, [A, B], [myList(A), B, relation([A, B, B]), B]).
foldRight(myNil, Accum, _, Accum).
foldRight(myCons(H, T), Accum, Relation, Result) :-
  foldRight(T, Accum, Relation, TailAccum),
  call(Relation, H, TailAccum, Result).

clausedef(myMap, [A, B], [myList(A), relation([A, B]), myList(B)]).
myMap(Input, Mapper, Output) :-
  foldRight(Input,
            myNil,
            lambda([A, CurList, myCons(B, CurList)],
                   call(Mapper, A, B)),
            Output).

clausedef(sum, [], [myList(int), int]).
sum(Input, Output) :-
  foldRight(Input,
            0,
            lambda([X, Y, Z],
                   Z is X + Y),
            Output).

clausedef(sumNative, [], [list(int), int]).
sumNative(Input, Output) :-
  sumNative(Input, 0, Output).

clausedef(sumNative, [], [list(int), int, int]).
sumNative([], Sum, Sum).
sumNative([H|T], Accum, Sum) :-
  NewAccum is Accum + H,
  sumNative(T, NewAccum, Sum).
```

The above example, if saved in the file `routines.pl`, should compile.
The above example shows the following parts:

1. Module definition, using `module`.
   This takes three components:
   1. The name of the module, which should match the filename
   2. The export list of procedures, which describe procedures which this module makes public to other modules.
      Any procedures defined in the file which are **not** part of this list are considered private to the module, and cannot be accessed from outside of the module.
      For example, in the above code `myMap/3` is public, but `foldRight/4` is private.
   3. The export list of datatypes, which describe datatypes which this modules makes public to other modules.
      All constructors of the given datatype are exported.
      Any datatypes defined in the file which are **not** part of this list are considered private to the module, and cannot be accessed from outside of the module.
      The only datatype in play in the example is `myList`, which is publicly exported.
2. Datatype definition, using `datadef`.
   This takes three components:
   1. The name of the datatype
   2. A list of generic types which are introduced in the datatype.
      If the datatype is not generic, an empty list (`[]`) must be used.
   3. The constructors for the datatype, which are permitted to be recursive.
3. Clause definition, using `clausedef`.
   While clause definitions may be located anywhere in the file, it is good practice to put them immediately before their corresponding clauses, as is done in the example.
   This takes three components:
   1. The name of the clause
   2. Any type variables the clause introduces.
      If the clause does not introduce any type variables, then an empty list (`[]`) must be used.
   3. The types of the parameters to the clause, specified in a list.
4. The `relation([...])` type, which is used for higher-order clauses.
   Note the use of square braces in the type.
   Values of type `relation([...])` are called with the `call` built-in, which behaves **substantially differently** from the typical `call` metaprogramming routine.
   Uses of `call` here ultimately are translated down into typical first-order clauses.
5. The `lambda` keyword, which introduces values of type `relation([...])`.
   These consist of two components:
   1. A parameter list, which is no more constrained than the parameter list for clauses.
      That is, unification is employed for the parameters, and they need not be variables (though they may be, if desired).
      Unlike with `clausedef`, the types of the parameters are inferred, and there is no way to explicitly annotate parameter types here.
   2. The body, which is a snippet of executable code.
      If multiple conjuncts are needed, they should be enclosed in an outer set of parentheses (as in, the body should be a functor with the name `,`).
6. The `int` type, which is short for `integer`.
   This behaves with `is` as one might expect.
7. The `list(...)` type, which is for the typical lists built into Prolog.
   These are referenced with the usual notation (e.g., `[H|T]`)

All procedures **must** have a `clausedef` associated with them.
