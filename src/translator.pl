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

% -PreviouslySeenVars: [Variable]
% -Terms: [Term]
% -VariablesUsed: [Variable]
% -NewTerms: [TranslatedTerm]
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
%
% We might have variables used which we haven't seen before.
% This essentially introduces variables.
translateTerms(_, [], [], [], Defs, Defs).
translateTerms(SeenVars, [H|T], Used, [HT|TT], Input, Output) :-
        translateTerm(SeenVars, H, HUsed, HT, Input, TempInput),
        setUnion(HUsed, SeenVars, NewSeenVars),
        translateTerms(NewSeenVars, T, TUsed, TT, TempInput, Output),
        append(HUsed, TUsed, DupUsed),
        makeDistinctEqual(DupUsed, Used).

% -PreviouslySeenVars: [Variable]
% -Term: Term
% -VariablesUsed: [Variable]
% -NewTerm: TranslatedTerm
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateTerm(_, Var, [Var], Var, Defs, Defs) :-
        var(Var),
        !.
translateTerm(_, Atom, [], Atom, Defs, Defs) :-
        atom(Atom),
        !.
translateTerm(_, Number, [], Number, Defs, Defs) :-
        number(Number),
        !.
translateTerm(SeenVars, lambda(Params, Body), LambdaCapturedUnique, Closure,
              Defs1, DefsFinal) :-
        !,
        % Translate each of the parameters to the lambda.  Variables
        % introduced here should not bleed into the outer scope, but
        % they will be needed in translating the body of the lambda
        translateTerms(SeenVars, Params, ParamsUsed, TranslatedParams,
                       Defs1, Defs2),
        translateBody(ParamsUsed, Body, BodyUsed, TranslatedBody,
                      Defs2, Defs3),
        
        % Introduce a lambda for this definition.  First, build up
        % the closure we're making, starting with the name.
        length(Params, Arity),
        freshLambdaLabel(Arity, Name),
        
        % The parameters to the closure are the variables which have been
        % closed over between the parameters and the body of the lambda.
        diffEqual(ParamsUsed, SeenVars, LambdaIntroduces),
        append(ParamsUsed, BodyUsed, LambdaUsed),
        diffEqual(LambdaUsed, LambdaIntroduces, LambdaCaptured),
        makeDistinctEqual(LambdaCaptured, LambdaCapturedUnique),
        Closure =.. [Name|LambdaCapturedUnique],

        % The corresponding code for the closure will take the
        % closure, along with the passed parameters to the call.
        callLambdaLabel(Arity, CallName),
        Head =.. [CallName, Closure|TranslatedParams],
        Defs3 = [:-(Head, TranslatedBody)|DefsFinal].
translateTerm(SeenVars, Structure, Used,
              TranslatedStructure, Defs1, DefsFinal) :-
        Structure =.. [Name|Params],
        !,
        translateTerms(SeenVars, Params, Used,
                       TranslatedParams, Defs1, DefsFinal),
        TranslatedStructure =.. [Name|TranslatedParams].

callLambdaLabel(Arity, Label) :-
        string_concat('call_lambda', Arity, CallNameString),
        atom_codes(Label, CallNameString).

% -Exps: [Exp]
% -VariablesUsed: [Variable]
% -TranslatedExps: [Exp]
translateExps(Exps, Used, Exps) :-
        % our expressions don't actually require any translation
        % for the moment; this is setup in case of future changes
        term_variables(Exps, Used).

% -PreviouslySeenVars: [Variable]
% -Bodies: [Body]
% -VariablesUsed: [Variable]
% -NewBodies: [TranslatedBody]
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateBodies(_, [], [], [], Defs, Defs).
translateBodies(SeenVars, [H|T], Used, [HT|TT], Input, Output) :-
        translateBody(SeenVars, H, HUsed, HT, Input, TempInput),
        setUnion(HUsed, SeenVars, NewSeenVars),
        translateBodies(NewSeenVars, T, TUsed, TT, TempInput, Output),
        append(HUsed, TUsed, DupUsed),
        makeDistinctEqual(DupUsed, Used).

% -PreviouslySeenVars: [Variable]
% -Body: Body
% -VariablesUsed: [Variable]
% -NewBody: TranslatedBody
% -HoistedDefsInput: [Clause]
% -HoistedDefsOutput: [Clause]
translateBody(_, Atom, [], Atom, Defs, Defs) :-
        atom(Atom),
        !.
translateBody(_, is(Exp1, Exp2), Used,
              is(TranslatedExp1, TranslatedExp2), Defs, Defs) :-
        !,
        translateExps([Exp1, Exp2], Used,
                      [TranslatedExp1, TranslatedExp2]).
translateBody(SeenVars, PairForm, Used,
              TranslatedPairForm, Defs1, DefsFinal) :-
        bodyPairForm(PairForm, Name, B1, B2),
        !,
        translateBodies(SeenVars, [B1, B2], Used,
                        NewBodies, Defs1, DefsFinal),
        TranslatedPairForm =.. [Name|NewBodies].
translateBody(SeenVars, HigherOrderCall, Used,
              TranslatedCall, Defs1, DefsFinal) :-
        HigherOrderCall =.. [call|Params],
        !,
        
        % Translate each of the parameters to the call
        translateTerms(SeenVars, Params, Used,
                       TranslatedParams, Defs1, DefsFinal),
        
        % replace this with a translated call
        length(Params, Arity),
        ArityWithoutLambda is Arity - 1,
        assertion(ArityWithoutLambda >= 0),
        callLambdaLabel(ArityWithoutLambda, CallName),
        TranslatedCall =.. [CallName|TranslatedParams].
translateBody(SeenVars, FirstOrderCall, Used,
              TranslatedCall, Defs1, DefsFinal) :-
        FirstOrderCall =.. [Name|Params],
        !,
        translateTerms(SeenVars, Params, Used,
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
        translateTerms([], Params, Used, TranslatedParams,
                       DefsInput, TempDefs),
        translateBody(Used, Body, _, TranslatedBody,
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

