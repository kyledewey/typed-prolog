Todo list:

1. Make things emit variables that aren't singletons
2. Actually write the code to a file
3. Order `call_lambda`s so that versions with the same arity are next to each other.
4. Add support for global variables which can be translated to different representations
   for different engines.
   
Done:

1. Get `is` working
2. Variables introduced inside of a lambda should not live beyond the scope
   of the lambda
3. Translation for lambdas.
