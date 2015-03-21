module(bootstrap_translator, [translateClauses/3], [engine_type]).

use_module('common.pl', [setUnion/3, setDifference/3, filter/3, setContains/2,
                         makeSetFromList/2, map/3, foldLeft/4], []).
use_module('bootstrap_syntax.pl', [],
                                  [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                                   typeConstructor, defdata, clauseclause, defglobalvar,
                                   defmodule, def_use_module, loadedFile]).

datadef(engine_type, [], [swipl, gnuprolog]).

globalvardef(counter, [], int).
globalvardef(engine, [], engine_type).

clausedef(freshInt, [], [int]).
freshInt(N) :-
        getvar(counter, N),
        NewN is N + 1,
        setvar(counter, NewN).

clausedef(yolo_UNSAFE_call_lambda_label, [], [int, atom]).
yolo_UNSAFE_call_lambda_label(Arity, Label) :-
        format(atom(Label), 'call_lambda~d', [Arity]).

clausedef(yolo_UNSAFE_fresh_lambda_label, [], [int, atom]).
yolo_UNSAFE_fresh_lambda_label(Arity, Label) :-
        freshInt(Int),
        format(atom(Label), 'lambda~d_~d', [Arity, Int]).

% Translates out lambdas and higher-order calls into auxilliary clauses and
% first-order calls, via defunctionalization.  Also translates out global variables
% into engine-specific forms.

clausedef(engineSetVarName, [], [engine_type, atom]).
engineSetVarName(swipl, nb_setval).
engineSetVarName(gnuprolog, g_assign).

clausedef(engineGetVarName, [], [engine_type, atom]).
engineGetVarName(swipl, nb_getval).
engineGetVarName(gnuprolog, g_read).

clausedef(yolo_UNSAFE_term_variables, [A], [A, list(int)]).
yolo_UNSAFE_term_variables(A, Variables) :-
        term_variables(A, Variables).

clausedef(translateMulti, [A, B], [list(int), % previously seen variables
                                   list(A), % what to translate
                                   list(int), % variables used
                                   list(B), % translated form
                                   list(clauseclause), % aux clauses input
                                   list(clauseclause), % aux clauses output
                                   relation([list(int), A, list(int), B,
                                             list(clauseclause), list(clauseclause)])]).
translateMulti(_, [], [], [], Defs, Defs, _).
translateMulti(SeenVars, [H|T], Used, [HT|TT], Input, Output, Trans) :-
        call(Trans, SeenVars, H, HUsed, HT, Input, TempInput),
        setUnion(HUsed, SeenVars, NewSeenVars),
        translateMulti(NewSeenVars, T, TUsed, TT, TempInput, Output, Trans),
        setUnion(HUsed, TUsed, Used).
                                             
clausedef(translateTerms, [], [list(int), % previously seen variables
                               list(term), % what to translate
                               list(int), % variables used in this term
                               list(term), % translated term
                               list(clauseclause), % aux clauses input
                               list(clauseclause)]). % aux clauses output
translateTerms(SeenVars, Terms, Used, NewTerms, Input, Output) :-
        translateMulti(SeenVars, Terms, Used, NewTerms, Input, Output,
                       lambda([A, B, C, D, E, F],
                              translateTerm(A, B, C, D, E, F))).

clausedef(translateBodies, [], [list(int), % previously seen variables
                                list(body), % what to translate
                                list(int), % variables used in this term
                                list(body), % translated term
                                list(clauseclause), % aux clauses input
                                list(clauseclause)]). % aux clauses output
translateBodies(SeenVars, Bodies, Used, NewBodies, Input, Output) :-
        translateMulti(SeenVars, Bodies, Used, NewBodies, Input, Output,
                       lambda([A, B, C, D, E, F],
                              translateBody(A, B, C, D, E, F))).

clausedef(translateBody, [], [list(int), % previously seen variables
                              body, % what to translate
                              list(int), % variables used in the body
                              body, % new body
                              list(clauseclause), % aux clauses input
                              list(clauseclause)]). % aux clauses output
translateBody(_, BodyIs, Used, BodyIs, Defs, Defs) :-
        BodyIs = body_is(_, _),
        !,
        % merely record which variables were used
        yolo_UNSAFE_term_variables(BodyIs, Used).
translateBody(SeenVars,
              body_setvar(Name, Term), Used,
              firstOrderCall(VarSetName, [term_constructor(Name, []), NewTerm]),
              Defs1, DefsFinal) :-
        !,
        getvar(engine, Engine),
        engineSetVarName(Engine, VarSetName),
        translateTerm(SeenVars, Term, Used, NewTerm, Defs1, DefsFinal).
translateBody(SeenVars,
              body_getvar(Name, Term), Used,
              firstOrderCall(VarGetName, [term_constructor(Name, []), NewTerm]),
              Defs1, DefsFinal) :-
        !,
        getvar(engine, Engine),
        engineGetVarName(Engine, VarGetName),
        translateTerm(SeenVars, Term, Used, NewTerm, Defs1, DefsFinal).
translateBody(SeenVars,
              bodyPair(B1, Op, B2), Used,
              bodyPair(NewB1, Op, NewB2),
              Defs1, DefsFinal) :-
        !,
        translateBodies(SeenVars, [B1, B2], Used, [NewB1, NewB2], Defs1, DefsFinal).
translateBody(SeenVars,
              higherOrderCall(What, Terms), Used, 
              firstOrderCall(CallName, NewTerms), Defs1, DefsFinal) :-
        !,

        % translate each of the parameters to the call
        translateTerms(SeenVars, [What|Terms], Used, NewTerms, Defs1, DefsFinal),

        % replace this with a translated call
        length(Terms, Arity),
        yolo_UNSAFE_call_lambda_label(Arity, CallName).
translateBody(SeenVars,
              firstOrderCall(Name, Terms), Used,
              firstOrderCall(Name, NewTerms),
              Defs1, DefsFinal) :-
        !,
        translateTerms(SeenVars, Terms, Used, NewTerms, Defs1, DefsFinal).

clausedef(translateTerm, [], [list(int), % previously seen variables
                              term, % what to translate
                              list(int), % variables used in this term
                              term, % translated term
                              list(clauseclause), % aux clauses input
                              list(clauseclause)]). % aux clauses output
translateTerm(_, term_var(X), [X], term_var(X), Clauses, Clauses) :- !.
translateTerm(_, term_num(N), [], term_num(N), Clauses, Clauses) :- !.
translateTerm(SeenVars,
              term_lambda(Params, Body), LambdaCapturedUnique,
              Closure,
              Defs1, DefsFinal) :-
        Closure = term_constructor(Name, LambdaCapturedUniqueVars),
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
        yolo_UNSAFE_fresh_lambda_label(Arity, Name),

        % The parameters to the closure are the variables which have been
        % closed over between the parameters and the body of the lambda.
        setDifference(ParamsUsed, SeenVars, LambdaIntroduces),
        append(ParamsUsed, BodyUsed, LambdaUsed),

        % variables that the lambda both captured and internally defined
        setDifference(LambdaUsed, LambdaIntroduces, LambdaCapturedAndUses),

        % only those variables which were captured from the outer scope
        filter(LambdaCapturedAndUses,
               lambda([Variable], setContains(SeenVars, Variable)),
               LambdaCaptured),
        makeSetFromList(LambdaCaptured, LambdaCapturedUnique),
        map(LambdaCapturedUnique,
            lambda([Variable, term_var(Variable)], true),
            LambdaCapturedUniqueVars),

        % The corresponding code for the closure will take the
        % closure, along with the passed parameters to the call.
        yolo_UNSAFE_call_lambda_label(Arity, CallName),
        NewClause = clauseclause(CallName,
                                 [Closure|TranslatedParams],
                                 TranslatedBody),
        Defs3 = [NewClause|DefsFinal].
translateTerm(SeenVars,
              term_constructor(Name, Params), Used,
              term_constructor(Name, TranslatedParams),
              Defs1, DefsFinal) :-
        !,
        translateTerms(SeenVars, Params, Used, TranslatedParams, Defs1, DefsFinal).

clausedef(translateClause, [], [clauseclause, clauseclause,
                                list(clauseclause), list(clauseclause)]).
translateClause(clauseclause(Name, Params, Body),
                clauseclause(Name, NewParams, NewBody),
                InputClauses, OutputClauses) :-
        translateTerms([], Params, Used, NewParams, InputClauses, TempClauses),
        translateBody(Used, Body, _, NewBody, TempClauses, OutputClauses).

clausedef(translateClauses, [], [list(clauseclause),
                                 engine_type,
                                 list(clauseclause)]).
translateClauses(Clauses, Engine, NewClauses) :-
        setvar(counter, 0),
        setvar(engine, Engine),
        foldLeft(Clauses, NewClauses,
                 lambda([Accum, Clause, NewAccum],
                        (translateClause(Clause, NewClause, Accum, TempAccum),
                         TempAccum = [NewClause|NewAccum])),
                 []).
