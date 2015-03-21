module(bootstrap_typechecker, [typecheckClauses/4], []).

use_module('bootstrap_syntax.pl', [],
                                  [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                                   typeConstructor, defdata, clauseclause, defglobalvar,
                                   defmodule, def_use_module, loadedFile]).
use_module('common.pl', [map/3, flatMap/3, zip/3, foldLeft/4, find/3,
                         atomContains/2, forall/2],
                        [pair, option]).

clausedef(builtinDataDefs, [], [list(defdata)]).
builtinDataDefs(
        [defdata(list, [A], [typeConstructor('.', [A, constructorType(list, [A])]),
                             typeConstructor([], [])])]).

% Even though these all are technically working with the same type variable,
% given that we'll copy terms this doesn't hurt anything
clausedef(builtinClauseDefs, [], [list(defclause)]).
builtinClauseDefs(
        [defclause(true, [], []),
         defclause(false, [], []),
         defclause(fail, [], []),
         defclause('!', [], []),
         defclause(var, [A], [A]),
         defclause(atom, [A], [A]),
         defclause(>, [], [intType, intType]),
         defclause(<, [], [intType, intType]),
         defclause(=<, [], [intType, intType]),
         defclause(>=, [], [intType, intType]),
         defclause(=, [A], [A, A]),
         defclause(\=, [A], [A, A]),
         defclause(==, [A], [A, A]),
         defclause(is_set, [A], [constructorType(list, [A])]),
         defclause(member, [A], [A, constructorType(list, [A])]),
         defclause(copy_term, [A], [A, A]),
         defclause(append, [A], [constructorType(list, [A]),
                                 constructorType(list, [A]),
                                 constructorType(list, [A])]),
         defclause(length, [A], [constructorType(list, [A]), intType]),
         defclause(atom_codes, [], [atomType, constructorType(list, [intType])])
        ]).

clausedef(keys, [A, B], [list(pair(A, B)), list(A)]).
keys(Pairs, Keys) :-
        map(Pairs, lambda([pair(Key, _), Key], true), Keys).

% succeeds if the provided mapping is unique
clausedef(mappingUnique, [A, B], [list(pair(A, B))]).
mappingUnique(Pairs) :-
        keys(Pairs, Keys),
        is_set(Keys).

% gets a mapping of constructor names to their corresponding data defs
clausedef(constructorToDataDefMapping, [], [list(defdata), list(pair(atom, defdata))]).
constructorToDataDefMapping(DefDatas, Mapping) :-
        flatMap(
            DefDatas,
            lambda([DefData, DefDataResult],
                (DefData = defdata(_, _, TypeConstructors),
                 map(TypeConstructors,
                     lambda([typeConstructor(Name, _), pair(Name, DefData)], true),
                     DefDataResult))),
            Mapping),
        mappingUnique(Mapping).

% gets a mapping of clauses with a given name and arity to their
% corresponding definitions
clausedef(clauseToClauseDefMapping, [], [list(defclause),
                                         list(pair(pair(atom, int), defclause))]).
clauseToClauseDefMapping(DefClauses, Mapping) :-
        map(DefClauses,
            lambda([DefClause, pair(pair(Name, Arity), DefClause)],
                (DefClause = defclause(Name, _, FormalParams),
                 length(FormalParams, Arity))),
            Mapping),
        mappingUnique(Mapping).

% holds data def mapping, clausedef mapping, and global var defs
datadef(state, [], [state(list(pair(atom, defdata)),
                          list(pair(pair(atom, int), defclause)),
                          list(defglobalvar))]).

clausedef(makeState, [], [list(defdata), list(defclause),
                          list(defglobalvar), state]).
makeState(DataDefs, ClauseDefs, GlobalVarDefs,
          state(DataDefMapping, ClauseDefMapping, GlobalVarDefs)) :-
        constructorToDataDefMapping(DataDefs, DataDefMapping),
        clauseToClauseDefMapping(ClauseDefs, ClauseDefMapping).

clausedef(expectedFormalParamTypes, [], [state, atom, int, list(type)]).
expectedFormalParamTypes(state(_, Mapping, _), Name, Arity, Expected) :-
        member(pair(pair(Name, Arity), defclause(_, _, RawExpected)), Mapping),
        copy_term(RawExpected, Expected).

% int is an uninstantiated variable
clausedef(envVariableType, [], [list(pair(int, type)), % input type environment
                                int, % variable
                                type, % variable's type
                                list(pair(int, type))]). % output type environment
envVariableType(TypeEnv, Variable, Type, NewTypeEnv) :-
        find(TypeEnv,
             lambda([pair(EnvVariable, _)], Variable == EnvVariable),
             FindResult),
        (FindResult = some(pair(_, EnvType)) ->
            (Type = EnvType,
             TypeEnv = NewTypeEnv);
            (NewTypeEnv = [pair(Variable, Type)|TypeEnv])).

clausedef(typecheckLhs, [], [list(pair(int, type)), % input type environment,
                             expLhs,
                             list(pair(int, type))]). % output type environment
typecheckLhs(TypeEnv, lhs_num(_), TypeEnv) :- !.
typecheckLhs(TypeEnv, lhs_var(Variable), NewTypeEnv) :-
        !,
        envVariableType(TypeEnv, Variable, intType, NewTypeEnv).

clausedef(typecheckExp, [], [list(pair(int, type)), % input type environment,
                             exp,
                             list(pair(int, type))]). % output type environment
typecheckExp(TypeEnv, exp_var(Variable), NewTypeEnv) :-
        !,
        envVariableType(TypeEnv, Variable, intType, NewTypeEnv).
typecheckExp(TypeEnv, exp_num(_), TypeEnv) :- !.
typecheckExp(TypeEnv, binop(E1, _, E2), NewTypeEnv) :-
        !,
        typecheckExp(TypeEnv, E1, TempTypeEnv), !,
        typecheckExp(TempTypeEnv, E2, NewTypeEnv).

clausedef(typecheckVarUse, [], [state,
                                list(pair(int, type)), % input type environment,
                                atom, % variable name
                                term, % setting / getting
                                list(pair(int, type))]). % output type environment
typecheckVarUse(State, TypeEnv, VarName, Term, NewTypeEnv) :-
        % determine what the expected type is
        State = state(_, _, GlobalVarDefs),
        member(defglobalvar(VarName, _, ExpectedType), GlobalVarDefs),

        % Ensure they line up.  We intentionally don't freshen type variables,
        % as all uses must be of matching types.
        typeofTerm(State, TypeEnv, Term, ExpectedType, NewTypeEnv).

clausedef(typecheckBody, [], [state,
                              list(pair(int, type)), % input type environment,
                              body,
                              list(pair(int, type))]). % output type environment
typecheckBody(_, TypeEnv, body_is(Lhs, Exp), NewTypeEnv) :-
        !,
        typecheckLhs(TypeEnv, Lhs, TempTypeEnv), !,
        typecheckExp(TempTypeEnv, Exp, NewTypeEnv), !.
typecheckBody(State, TypeEnv, body_setvar(VarName, Term), NewTypeEnv) :-
        !,
        typecheckVarUse(State, TypeEnv, VarName, Term, NewTypeEnv), !.
typecheckBody(State, TypeEnv, body_getvar(VarName, Term), NewTypeEnv) :-
        !,
        typecheckVarUse(State, TypeEnv, VarName, Term, NewTypeEnv), !.
typecheckBody(State, TypeEnv, bodyPair(B1, _, B2), NewTypeEnv) :-
        !,
        typecheckBody(State, TypeEnv, B1, TempTypeEnv), !,
        typecheckBody(State, TempTypeEnv, B2, NewTypeEnv), !.
typecheckBody(State, TypeEnv, higherOrderCall(What, ActualParams), NewTypeEnv) :-
        !,
        typeofTerm(State, TypeEnv, What, relationType(FormalParams), TempTypeEnv), !,
        typeofTerms(State, TempTypeEnv, ActualParams, FormalParams, NewTypeEnv), !.
typecheckBody(State, TypeEnv, firstOrderCall(Name, ActualParams), NewTypeEnv) :-
        length(ActualParams, Arity),
        expectedFormalParamTypes(State, Name, Arity, FormalParams), !,
        typeofTerms(State, TypeEnv, ActualParams, FormalParams, NewTypeEnv), !.

clausedef(typeofTerm, [], [state,
                           list(pair(int, type)), % input type environment
                           term, type,
                           list(pair(int, type))]). % output type environment
typeofTerm(_, TypeEnv, term_var(Variable), Type, NewTypeEnv) :-
        envVariableType(TypeEnv, Variable, Type, NewTypeEnv).
typeofTerm(_, TypeEnv, term_num(_), intType, TypeEnv).
typeofTerm(State, TypeEnv, term_lambda(Params, Body), relationType(Types), TypeEnv) :-
        !,
        typeofTerms(State, TypeEnv, Params, Types, TempTypeEnv),
        typecheckBody(State, TempTypeEnv, Body, _).
typeofTerm(State, TypeEnv,
           term_constructor(ConstructorName, ConstructorActualParams),
           constructorType(TypeName, TypeParams),
           NewTypeEnv) :-

        % figure out which datadef is in play
        State = state(Mapping, _, _),
        member(pair(ConstructorName, RawDefData), Mapping),
        copy_term(RawDefData, defdata(TypeName, TypeParams, Constructors)),

        % figure out which constructor is in play
        member(typeConstructor(ConstructorName, ConstructorFormalParams), Constructors),
        
        % make sure the types line up
        !, % we found one - no going back to treating it like an atom
        typeofTerms(State, TypeEnv,
                    ConstructorActualParams,
                    ConstructorFormalParams,
                    NewTypeEnv), !.
typeofTerm(_, TypeEnv, term_constructor(_, []), atomType, TypeEnv).

% int is really an uninstantiated variable
clausedef(typeofTerms, [], [state,
                            list(pair(int, type)), % input type environment
                            list(term),
                            list(type),
                            list(pair(int, type))]). % output type environment
typeofTerms(State, TypeEnv, Terms, Types, NewTypeEnv) :-
        zip(Terms, Types, Zipped),
        foldLeft(Zipped, TypeEnv,
                 lambda([Accum, pair(Term, Type), NewAccum],
                        typeofTerm(State, Accum, Term, Type, NewAccum)),
                 NewTypeEnv), !.

% succeeds if the given clause has been marked unsafe
clausedef(markedUnsafe, [], [atom]).
markedUnsafe(Name) :-
        atomContains(Name, 'yolo_UNSAFE_').

clausedef(typecheckClause, [], [state, clauseclause]).
typecheckClause(State, clauseclause(Name, FormalParams, Body)) :-
        length(FormalParams, Arity),
        expectedFormalParamTypes(State, Name, Arity, Expected),
        typeofTerms(State, [], FormalParams, Expected, TypeEnv),
        (markedUnsafe(Name) ->
            true;
            typecheckBody(State, TypeEnv, Body, _)), !.

% will add in builtins itself
clausedef(typecheckClauses, [], [list(defdata), list(defclause),
                                 list(defglobalvar), list(clauseclause)]).
typecheckClauses(UserDataDefs, UserClauseDefs, UserDefGlobalVars, Clauses) :-
        % add in builtins
        builtinDataDefs(BuiltinDataDefs),
        builtinClauseDefs(BuiltinClauseDefs),
        append(BuiltinDataDefs, UserDataDefs, DataDefs),
        append(BuiltinClauseDefs, UserClauseDefs, ClauseDefs),

        % perform typechecking
        makeState(DataDefs, ClauseDefs, UserDefGlobalVars, State),
        forall(Clauses, lambda([Clause], typecheckClause(State, Clause))).
