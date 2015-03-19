Todo list:

1. Fix bug where we will allow a type definition that doesn't use all the type
   parameters in a definition, as with just plain `list`.  This is recognized as
   a valid type, though attempts to use it won't work (as expected).

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
