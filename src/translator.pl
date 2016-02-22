module(translator, [translateClauses/3], [engine_type]).

use_module('common.pl', [setUnion/3, setDifference/3, filter/3, setContains/2,
                         makeSetFromList/2, map/3, foldLeft/4, sortItems/4,
                         flatMap/3, beginsWith/2, makeSetFromList/2, notMember/2,
                         setsOverlap/2, existsOnce/2],
                        [pair, option]).
use_module('syntax.pl', [],
                        [op, exp, expLhs, term, body, type, defclause,
                         typeConstructor, defdata, clauseclause, defglobalvar,
                         defmodule, def_use_module, loadedFile, compareOp]).

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
translateBody(_, BodyComparison, Used, BodyComparison, Defs, Defs) :-
        BodyComparison = bodyComparison(_, _, _),
        !,
        % merely record which variables were used
        yolo_UNSAFE_term_variables(BodyComparison, Used).
translateBody(SeenVars,
              bodyUnary(Op, Body), Used,
              bodyUnary(Op, NewBody),
              Defs1, DefsFinal) :-
        !,
        translateBody(SeenVars, Body, Used, NewBody, Defs1, DefsFinal).
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
              TranslatedCall,
              Defs1, DefsFinal) :-
        !,
        getvar(engine, Engine),
        translateTerms(SeenVars, Terms, Used, NewTerms, Defs1, DefsFinal),
        translateCall(Engine, Name, NewTerms, TranslatedCall).

clausedef(translateCall, [], [engine_type, % what engine we're under
                              atom, % what is called
                              list(term), % translated terms passed to the call
                              body]). % resulting body
translateCall(swipl, fd_labeling, Terms, firstOrderCall('label', Terms)) :- !.
translateCall(_, Name, Terms, firstOrderCall(Name, Terms)).

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
        setUnion(SeenVars, ParamsUsed, SeenForBody),
        translateBody(SeenForBody, Body, BodyUsed, TranslatedBody,
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

% assumes that we have a purely first-order program.  That is, translation
% has already occurred.
clausedef(bodyDirectlyCalls, [], [body, % the body to check
                                  list(pair(atom, int)), % input diff list
                                  list(pair(atom, int))]). % output diff list
bodyDirectlyCalls(body_is(_, _), List, List).
bodyDirectlyCalls(body_setvar(_, _), List, List).
bodyDirectlyCalls(body_getvar(_, _), List, List).
bodyDirectlyCalls(bodyComparison(_, _, _), List, List).
bodyDirectlyCalls(bodyUnary(_, Body), Input, Output) :-
        bodyDirectlyCalls(Body, Input, Output).
bodyDirectlyCalls(bodyPair(Body1, _, Body2), Input, Output) :-
        bodyDirectlyCalls(Body1, Input, Temp),
        bodyDirectlyCalls(Body2, Temp, Output).
bodyDirectlyCalls(firstOrderCall(Name, Params), [pair(Name, Arity)|Rest], Rest) :-
        length(Params, Arity).

clausedef(bodyDirectlyCalls, [], [body, list(pair(atom, int))]).
bodyDirectlyCalls(Body, Calls) :-
        bodyDirectlyCalls(Body, Calls, []).

clausedef(clauseNameArity, [], [clauseclause, atom, int]).
clauseNameArity(clauseclause(Name, Params, _), Name, Arity) :-
        length(Params, Arity).

clausedef(callsUnknownLambda, [], [list(pair(atom, int)), % what is called
                                   list(atom)]). % existing lambdas
callsUnknownLambda(CalledClauses, ExistingLambdas) :-
        existsOnce(CalledClauses,
                   lambda([pair(Name, _)],
                          (isCallLambda(Name),
                           notMember(Name, ExistingLambdas)))).

clausedef(trimDeadClauses, [], [list(clauseclause), % input clauses
                                list(pair(pair(atom, int), list(pair(atom, int)))), % called mapping
                                list(atom), % called lambdas
                                list(pair(atom, int)), % blacklist - call anything here and die
                                list(clauseclause), % output clauses accumulator
                                list(clauseclause)]). % output clauses
trimDeadClauses([], _, _, _, Accum, RevAccum) :-
        reverse(Accum, RevAccum).
trimDeadClauses([H|T], Mapping, CalledLambdas, Blacklist, Accum, Output) :-
        clauseNameArity(H, Name, Arity),
        Key = pair(Name, Arity),
        member(pair(Key, ClauseCalls), Mapping),
        ((setsOverlap(ClauseCalls, Blacklist);
          callsUnknownLambda(ClauseCalls, CalledLambdas)) ->
            % add it to the blacklist and restart
            (reverse(Accum, RevAccum),
             append(RevAccum, T, NewInput),
             trimDeadClauses(NewInput, Mapping, CalledLambdas, [Key|Blacklist],
                             [], Output));
            (trimDeadClauses(T, Mapping, CalledLambdas, Blacklist, [H|Accum], Output))).

clausedef(isCallLambda, [], [atom]).
isCallLambda(Name) :-
        atom_codes('call_lambda', CallList),
        atom_codes(Name, NameList),
        beginsWith(NameList, CallList).

clausedef(clauseCallsMapping, [], [list(clauseclause),
                                   list(pair(pair(atom, int), list(pair(atom, int))))]).
clauseCallsMapping(InputClauses, Mapping) :-
        map(InputClauses,
            lambda([clauseclause(Name, Params, Body),
                    pair(pair(Name, Arity), ClauseMapping)],
                   (length(Params, Arity),
                    bodyDirectlyCalls(Body, ClauseMapping))),
            Mapping).

clausedef(makeDirective, [], [term, clauseclause]).
makeDirective(Term, clauseclause(':-', [Term], firstOrderCall('true', []))).

clausedef(clpOperator, [], [compareOp]).
clpOperator(clp_lt).
clpOperator(clp_lte).
clpOperator(clp_gt).
clpOperator(clp_gte).
clpOperator(clp_eq).
clpOperator(clp_neq).

clausedef(bodyUsesClp, [], [body]).
bodyUsesClp(bodyUnary(_, Body)) :-
    bodyUsesClp(Body).
bodyUsesClp(bodyPair(Body1, _, Body2)) :-
    (bodyUsesClp(Body1); bodyUsesClp(Body2)),
    !.
bodyUsesClp(bodyComparison(_, Op, _)) :-
    clpOperator(Op).

clausedef(handleClp, [], [engine_type,
                          list(clauseclause),
                          list(clauseclause)]).
handleClp(swipl, InputClauses, OutputClauses) :-
    (existsOnce(InputClauses,
                lambda([clauseclause(_, _, Body)], bodyUsesClp(Body))) ->
         (makeDirective(
                term_constructor('use_module',
                                 [term_constructor('library',
                                                   [term_constructor('clpfd', [])])]),
                UseClpfd),
          OutputClauses = [UseClpfd|InputClauses]);
         (InputClauses = OutputClauses)).
handleClp(gnuprolog, Clauses, Clauses).

% Currently, everything could be an entry point, so this is very inexact.
% If we see a call to a lambda clause that doesn't exist, then we trim
% out the clause.  We recursively trim out things that call those.
clausedef(trimDeadClauses, [], [list(pair(pair(atom, int), list(pair(atom, int)))),
                                list(clauseclause), % input clauses
                                list(clauseclause)]). % output clauses
trimDeadClauses(Mapping, InputClauses, OutputClauses) :-
        % figure out which lambdas are in play
        flatMap(InputClauses,
                lambda([clauseclause(Name, _, _), Result],
                       (isCallLambda(Name) ->
                           (Result = [Name]);
                           (Result = []))),
                RawCalledLambdas),
        makeSetFromList(RawCalledLambdas, CalledLambdas),

        % If something calls a lambda not in that list, throw it out.
        % We then need to throw out everything else that calls that.
        trimDeadClauses(InputClauses, Mapping, CalledLambdas, [], [], OutputClauses).

clausedef(translateClauses, [], [list(clauseclause),
                                 engine_type,
                                 list(clauseclause)]).
translateClauses(Clauses, Engine, FinalClauses) :-
        setvar(counter, 0),
        setvar(engine, Engine),
        foldLeft(Clauses, pair(UserClauses, AuxClauses),
                 lambda([pair([TransClause|RestUser], CurAux),
                         Clause,
                         pair(RestUser, RestAux)],
                        translateClause(Clause, TransClause, CurAux, RestAux)),
                 pair([], [])),
        sortItems(AuxClauses,
                  lambda([clauseclause(Name, _, _), Name], true),
                  lambda([Name1, Name2], Name1 @> Name2),
                  SortedAuxClauses),
        append(SortedAuxClauses, UserClauses, UntrimmedClauses),
        clauseCallsMapping(UntrimmedClauses, Mapping),
        trimDeadClauses(Mapping, UntrimmedClauses, TrimmedClauses),
        handleClp(Engine, TrimmedClauses, FinalClauses).
