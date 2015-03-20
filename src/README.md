Todo list:

1. Fix bug where we will allow a type definition that doesn't use all the type
   parameters in a definition, as with just plain `list`.  This is recognized as
   a valid type, though attempts to use it won't work (as expected).
2. Get rid of the course-grained `yolo_UNSAFE`.  Replace it with two separate mechanisms:
    1. A raw call, wherein we escape directly into Prolog.  We provide explicit types for
       what the raw call does, where in a worst case we can provide type parameters for
       everything.  This allows us to do more typechecking than we currently do.
    2. A raw term, for which no name mangling occurs.  Something like
       `yolo_UNSAFE_raw_term(foo(1))`.  Everything within is passed along-as is.
       If you want to combine raw and non-raw terms, then you'll have to use variables
       as intermediaries, as with `Cooked = cooked(1, 2), Raw = yolo_UNSAFE_raw_term(foo(Cooked))`.
3. Allow for blanket imports.
4. User-facing syntax relations should just be `relation(A, B)`, not `relation([A, B])`

Done:

1. Get `is` working
2. Variables introduced inside of a lambda should not live beyond the scope
   of the lambda
3. Translation for lambdas.
4. Make things emit variables that aren't singletons
5. Actually write the code to a file
6. Order `call_lambda`s so that versions with the same arity are next to each other.
7. Add support for global variables which can be translated to different representations
   for different engines.
8. Add support for modules.
