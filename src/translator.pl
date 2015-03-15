% Our lambdas need to be translated into something stock Prolog understands.
% Instead of relying upon call and giving up performance, we will employ
% defunctionalization.

%% test :-
%%         A = 5,
%%         T = lambda([B, C],
%%             C = lambda([Z], Z is A + B)).

%% translatedTest :-
%%         A = 5,
%%         T = lambda2_0(A).

%% call_lambda2(lambda2_0(A), B, C) :-
%%         C = lambda1_0(A, B).
%% call_lambda1(lambda1_0(A, B), Z) :-
%%         Z is A + B.

% -ConcreteList: [A]
% -Input:        [A]
% -Output:       [A]
appendDiffList([], List, List).
appendDiffList([H|T], [H|Temp], Output) :-
        appendDiffList(T, Temp, Output).

% -InScopeVariables:    [Variable]
% -Terms:               [Term]
% -FreeVariablesInput:  [Variable]
% -FreeVariablesOutput: [Variable]
freeVariablesInTerms(InScope, [], List, List).
freeVariablesInTerms(InScope, [H|T], Input, Output) :-
        freeVariablesInTerm(InScope, H, Input, Temp),
        freeVariablesInTerms(InScope, T, Temp, Output).

% -InScopeVariables:    [Variable]
% -Term:                Term
% -FreeVariablesInput:  [Variable]
% -FreeVariablesOutput: [Variable]
%
% Gets all the variables which are free in the given term.
% FreeVariablesInput and FreeVariablesOutput form a difference list.
%
freeVariablesInTerm(InScope, Var, Input, Output) :-
        var(Var),
        !,
        (memberEqual(Var, InScope) ->
            Input = Output;
            Input = [Var|Output]).
freeVaraiblesInTerm(InScope, Atom, List, List) :-
        atom(Atom),
        !.
freeVaraiblesInTerm(InScope, Num, List, List) :-
        number(Num),
        !.
freeVaraiblesInTerm(InScope, lambda(Params, Body), Input, Output) :-
        !,
        % Get the variables specifically for the parameters.
        % These should not be considered free for the body.
        freeVariablesInTerms(InScope, Params, FreeInParams, []),

        append(FreeInParams, InScope, BodyInScope),
        freeVariablesInBody(BodyInScope, Body, Input, Output).
freeVariablesInTerms(InScope, Structure, Input, Output) :-
        
% STOPPED HERE.
% "Free variables" doesn't have quite the same meaning as it does in most
% other languages, because here we can introduce a new variable at any time.
% For our purposes, it's more interesting to thing of these as captured
% variables.  We note in a lambda when a variable is used that already
% exists in the outer environment.