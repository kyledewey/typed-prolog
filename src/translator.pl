:- module('translator', [translateClauses/2]).

:- use_module('util.pl').

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

% Consider the following two independent code snippets:
%
% Header = lambda(..., ...Header...)
% lambda(..., ...Header...) = Header
%
% From a Prolog standpoint, these two snippets should do the exact
% same thing.  However, from the perspective of our type system,
% implicitly they don't.  This is because in our type system, variables
% introduced in a lambda stay in the lambda, _even if_ those same variables
% are used in the clause.  This is done intentionally (and needs extra
% code to happen this way) because this is really bizarre coming from a
% functional standpoint: without this extra bit, we'd have to look at the
% whole clause to figure out what variables the lambda captures, as opposed
% to just the usual case of variables defined before the lambda.  I see
% the second case above as a case where we'd have to look at later code,
% so for the purposes of translation, the first snippet captures Header,
% and the second doesn't.

freshInt(Counter) :-
        nb_getval(counter, Counter),
        NewCounter is Counter + 1,
        nb_setval(counter, NewCounter).

% -Prefix: Prefix for the label
% -Label: Atom
freshLabel(Prefix, Label) :-
        freshInt(Int),
        string_concat(Prefix, Int, Concat),
        atom_codes(Label, Concat).

freshLambdaLabel(Arity, Label) :-
        string_concat('lambda', Arity, Temp1),
        string_concat(Temp1, '_', Temp2),
        freshLabel(Temp2, Label).

% -VariablesInScope: [Variable]
% -Terms: [Term]
% -NewVariablesInScope: [Variable]
% -VariablesInScopeUsed: [Variable]
% -NewTerms: [TranslatedTerm]
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateTerms(InScope, [], InScope, [], [], Defs, Defs).
translateTerms(InScope, [H|T], NewInScope, Used, [HT|TT], Input, Output) :-
        translateTerm(InScope, H, TempInScope, HUsed, HT, Input, TempInput),
        translateTerms(TempInScope, T, NewInScope, TUsed, TT, TempInput, Output),

        % We only care about stuff that was used in the original scope
        append(HUsed, TUsed, TempUsed),
        diffEqual(NewInScope, InScope, DiffScopes),
        diffEqual(TempUsed, DiffScopes, Used).

% -VariablesInScope: [Variable]
% -Term: Term
% -NewVariablesInScope: [Variable]
% -VariablesInScopeUsed: [Variable]
% -NewTerm: TranslatedTerm
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateTerm(InScope, Var, NewInScope, Used, NewVar, Defs, Defs) :-
        var(Var),
        !,
        Var = NewVar,
        (memberEqual(Var, InScope) ->
            (NewInScope = InScope,
             Used = [Var]);
            (NewInScope = [Var|InScope],
             Used = [])).
translateTerm(InScope, Atom, InScope, [], Atom, Defs, Defs) :-
        atom(Atom),
        !.
translateTerm(InScope, Number, InScope, [], Number, Defs, Defs) :-
        number(Number),
        !.
translateTerm(InScope, lambda(Params, Body), InScope, Used, Closure,
              Defs1, DefsFinal) :-
        !,
        % Translate each of the parameters to the lambda.  Variables
        % introduced here should not bleed into the outer scope, but
        % they will be needed in translating the body of the lambda
        translateTerms(InScope, Params, LambdaParamsInScope,
                       LambdaParamsUsed, TranslatedParams, Defs1, Defs2),
        translateBody(LambdaParamsInScope, Body, Used,
                      LambdaBodyUsed, TranslatedBody, Defs2, Defs3),
        
        % Introduce a lambda for this definition.  First, build up
        % the closure we're making, starting with the name.
        length(Params, Arity),
        freshLambdaLabel(Arity, Name),
        
        % The parameters to the closure are the variables which have been
        % closed over between the parameters and the body of the lambda.
        append(LambdaParamsUsed, LambdaBodyUsed, LambdaUsed),
        diffEqual(LambdaUsed, LambdaParamsInScope, LambdaCaptured),
        makeDistinctEqual(LambdaCaptured, LambdaCapturedUnique),
        format('LAMBDA USED: ~w~nLAMBDA PARAMS IN SCOPE: ~w~nLAMBDA CAPTURED: ~w~n', [LambdaUsed, LambdaParamsInScope, LambdaCaptured]),
        Closure =.. [Name|LambdaCapturedUnique],

        % The corresponding code for the closure will take the
        % closure, along with the passed parameters to the call.
        callLambdaLabel(Arity, CallName),
        Head =.. [CallName, Closure|TranslatedParams],
        Defs3 = [:-(Head, TranslatedBody)|DefsFinal].
translateTerm(InScope, Structure, NewInScope, Used,
              TranslatedStructure, Defs1, DefsFinal) :-
        Structure =.. [Name|Params],
        !,
        translateTerms(InScope, Params, NewInScope, Used,
                       TranslatedParams, Defs1, DefsFinal),
        TranslatedStructure =.. [Name|TranslatedParams].

callLambdaLabel(Arity, Label) :-
        string_concat('call_lambda', Arity, CallNameString),
        atom_codes(Label, CallNameString).

% -VariablesInScope: [Variable]
% -Exps: [ArithmeticExpresion]
% -NewVariablesInScope: [Variable]
% -VariablesInScopeUsed: [Variable]
% -NewExps: [ArithmeticExpression]
translateExps(_, [], [], [], []).
translateExps(InScope, [H|T], NewInScope, Used, [HT|TT]) :-
        translateExp(InScope, H, TempInScope, HUsed, HT),
        translateExps(TempInScope, T, NewInScope, RestUsed, TT),
        append(HUsed, RestUsed, Used).

% -VariablesInScope: [Variable]
% -Exp: ArithmeticExpresion
% -NewVariablesInScope: [Variable]
% -VariablesInScopeUsed: [Variable]
% -NewExp: ArithmeticExpression
translateExp(InScope, Exp, NewInScope, Used, Exp) :-
        % our expressions don't actually require any translation
        % for the moment; this is setup in case of future changes
        term_variables(Exp, VarsInExp),
        diffEqual(VarsInExp, InScope, NewInScope),
        diffEqual(VarsInExp, NewInScope, Used).
        
% -VariablesInScope: [Variable]
% -Bodies: [Body]
% -NewVariablesInScope: [Variable]
% -VariablesInScopeUsed: [Variable]
% -NewBodies: [TranslatedBody]
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateBodies(InScope, [], InScope, [], [], Defs, Defs).
translateBodies(InScope, [H|T], NewInScope, Used, [HT|TT], Input, Output) :-
        translateBody(InScope, H, TempInScope, HUsed, HT, Input, TempInput),
        translateBodies(TempInScope, T, NewInScope, TUsed, TT, TempInput, Output),

        % We only care about stuff that was used in the original scope
        append(HUsed, TUsed, TempUsed),
        diffEqual(NewInScope, InScope, DiffScopes),
        diffEqual(TempUsed, DiffScopes, Used).

% -VariablesInScope: [Variable]
% -Body: Body
% -NewVariablesInScope: [Variable]
% -VariablesInScopeUsed: [Variable]
% -NewBody: TranslatedBody
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateBody(InScope, Atom, InScope, [], Atom, Defs, Defs) :-
        atom(Atom),
        !.
translateBody(InScope, is(Exp1, Exp2), NewInScope, Used,
              is(TranslatedExp1, TranslatedExp2), Defs, Defs) :-
        !,
        translateExps(InScope, [Exp1, Exp2], NewInScope, Used,
                      [TranslatedExp1, TranslatedExp2]).
translateBody(InScope, PairForm, NewInScope, Used,
              TranslatedPairForm, Defs1, DefsFinal) :-
        bodyPairForm(PairForm, Name, B1, B2),
        !,
        translateBodies(InScope, [B1, B2], NewInScope, Used,
                        NewBodies, Defs1, DefsFinal),
        TranslatedPairForm =.. [Name|NewBodies].
translateBody(InScope, HigherOrderCall, NewInScope, Used,
              TranslatedCall, Defs1, DefsFinal) :-
        HigherOrderCall =.. [call|Params],
        !,
        
        % Translate each of the parameters to the call
        translateTerms(InScope, Params, NewInScope, Used,
                       TranslatedParams, Defs1, DefsFinal),
        
        % replace this with a translated call
        length(Params, Arity),
        ArityWithoutLambda is Arity - 1,
        assertion(ArityWithoutLambda >= 0),
        callLambdaLabel(ArityWithoutLambda, CallName),
        TranslatedCall =.. [CallName|TranslatedParams].
translateBody(InScope, FirstOrderCall, NewInScope, Used,
              TranslatedCall, Defs1, DefsFinal) :-
        FirstOrderCall =.. [Name|Params],
        !,
        translateTerms(InScope, Params, NewInScope, Used,
                       TranslatedParams, Defs1, DefsFinal),
        TranslatedCall =.. [Name|TranslatedParams].

denormalizeClause(:-(Head, true), Head) :- !.
denormalizeClause(Clause, Clause).

% -Clause: Clause
% -NewClause: Clause
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateClause(:-(Head, Body), NewClause, DefsInput, DefsOutput) :-
        Head =.. [Name|Params],
        translateTerms([], Params, InScope, _, TranslatedParams,
                       DefsInput, TempDefs),
        translateBody(InScope, Body, _, _, TranslatedBody,
                      TempDefs, DefsOutput),
        NewHead =.. [Name|TranslatedParams],
        denormalizeClause(:-(NewHead, TranslatedBody), NewClause).

% -Clauses: [Clause]
% -NewClauses: [Clause]
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateClauses([], [], Defs, Defs).
translateClauses([H|T], [HT|TT], Input, Output) :-
        translateClause(H, HT, Input, Temp),
        translateClauses(T, TT, Temp, Output).

% -Clauses: [Clause]
% -NewClauses: [Clause]
translateClauses(Clauses, NewClauses) :-
        nb_setval(counter, 0),
        translateClauses(Clauses, MainClauses, AuxClauses, []),
        append(MainClauses, AuxClauses, NewClauses).

