module(typechecker, [typecheckClauses/4], []).

use_module('syntax.pl', [],
                        [op, exp, expLhs, term, body, type, defclause,
                         typeConstructor, defdata, clauseclause, defglobalvar,
                         defmodule, def_use_module, loadedFile]).
use_module('common.pl', [map/3, flatMap/3, zip/3, foldLeft/4, find/3,
                         atomContains/2, forall/2, onFailure/2,
                         yolo_UNSAFE_format_shim/2, duplicates/2],
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
         defclause(nonvar, [A], [A]),
         defclause(ground, [A], [A]),
         defclause(@>, [A], [A, A]),
         defclause(@<, [A], [A, A]),
         defclause(@=<, [A], [A, A]),
         defclause(@>=, [A], [A, A]),
         defclause(=, [A], [A, A]),
         defclause(unify_with_occurs_check, [A], [A, A]),
         defclause(\=, [A], [A, A]),
         defclause(fd_labeling, [], [constructorType(list, [intType])]),
         defclause(==, [A], [A, A]),
         defclause(\==, [A], [A, A]),
         defclause(is_set, [A], [constructorType(list, [A])]),
         defclause(member, [A], [A, constructorType(list, [A])]),
         defclause(reverse, [A], [constructorType(list, [A]), constructorType(list, [A])]),
         defclause(copy_term, [A], [A, A]),
         defclause(append, [A], [constructorType(list, [A]),
                                 constructorType(list, [A]),
                                 constructorType(list, [A])]),
         defclause(length, [A], [constructorType(list, [A]), intType]),
         defclause(atom_codes, [], [atomType, constructorType(list, [intType])]),
         defclause(atom_number, [], [atomType, intType])
        ]).

clausedef(keys, [A, B], [list(pair(A, B)), list(A)]).
keys(Pairs, Keys) :-
        map(Pairs, lambda([pair(Key, _), Key], true), Keys).

clausedef(ensureUnique, [A], [list(A), % items
                              atom]).  % error message if not
ensureUnique(Items, ErrorMessage) :-
    duplicates(Items, Duplicates),
    onFailure(
            lambda([], Duplicates = []),
            lambda([], yolo_UNSAFE_format_shim(ErrorMessage, [Duplicates]))).
    
% succeeds if the provided mapping is unique
clausedef(mappingUnique, [A, B], [list(pair(A, B)), % mapping
                                  atom]).           % error message if not
mappingUnique(Pairs, ErrorMessage) :-
    keys(Pairs, Keys),
    ensureUnique(Keys, ErrorMessage).

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
        % NOTE: this does not appear in practice as module rewriting
        % catches this first.
        mappingUnique(Mapping, 'Duplicate locally-defined constructor names: ~w~n~n').

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
        % TODO: should this test be moved to module rewriting?
        % A similar test for constructors is already handled there
        % (see `constructorToDataDefMapping` for details)
        mappingUnique(Mapping, 'Duplicate clausedefs for: ~w~n~n').

% holds data def mapping, clausedef mapping, and global var defs
datadef(state, [], [state(list(pair(atom, defdata)),
                          list(pair(pair(atom, int), defclause)),
                          list(defglobalvar))]).

clausedef(ensureTypeNamesUnique, [], [list(defdata)]).
ensureTypeNamesUnique(DataDefs) :-
    map(DataDefs,
        lambda([defdata(Name, _, _), Name], true),
        TypeNames),
    ensureUnique(TypeNames, 'Duplicate type names: ~w~n~n').
    
clausedef(makeState, [], [list(defdata), list(defclause),
                          list(defglobalvar), state]).
makeState(DataDefs, ClauseDefs, GlobalVarDefs,
          state(DataDefMapping, ClauseDefMapping, GlobalVarDefs)) :-
    ensureTypeNamesUnique(DataDefs),
    constructorToDataDefMapping(DataDefs, DataDefMapping),
    clauseToClauseDefMapping(ClauseDefs, ClauseDefMapping).

clausedef(expectedFormalParamTypes, [], [state, % current type state
                                         atom, % name of the clause
                                         int, % arity of the clause
                                         list(type), % generics involved in the
                                                     % expected types
                                         list(type)]). % expected types
expectedFormalParamTypes(state(_, Mapping, _), Name, Arity, Generics, Expected) :-
        member(pair(pair(Name, Arity), RawClause), Mapping),
        copy_term(RawClause, defclause(_, Generics, Expected)).

% just ignores the generics
clausedef(expectedFormalParamTypes, [], [state, atom, int, list(type)]).
expectedFormalParamTypes(State, Name, Arity, Expected) :-
        expectedFormalParamTypes(State, Name, Arity, _, Expected).

% int is an uninstantiated variable
clausedef(envVariableType, [], [list(pair(int, type)), % input type environment
                                int, % variable
                                type, % variable's type
                                list(pair(int, type))]). % output type environment
envVariableType(TypeEnv, Variable, Type, NewTypeEnv) :-
        find(TypeEnv,
             lambda([pair(EnvVariable, _)], Variable == EnvVariable),
             FindResult),
        !, % if we were to backtrack, we'd always be able to add a new
           % variable
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
typecheckExp(TypeEnv, unaryop(_, E), NewTypeEnv) :-
        !,
        typecheckExp(TypeEnv, E, NewTypeEnv).

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
typecheckBody(State, TypeEnv, Body, NewTypeEnv) :-
        onFailure(
            lambda([], rawTypecheckBody(State, TypeEnv, Body, NewTypeEnv)),
            lambda([], yolo_UNSAFE_format_shim('Type error at body ~w~n~n', [Body]))).

clausedef(rawTypecheckBody, [], [state,
                                 list(pair(int, type)), % input type environment,
                                 body,
                                 list(pair(int, type))]). % output type environment
rawTypecheckBody(_, TypeEnv, body_is(Lhs, Exp), NewTypeEnv) :-
        !,
        typecheckLhs(TypeEnv, Lhs, TempTypeEnv), !,
        typecheckExp(TempTypeEnv, Exp, NewTypeEnv), !.
rawTypecheckBody(_, TypeEnv, bodyComparison(Exp1, _, Exp2), NewTypeEnv) :-
        !,
        typecheckExp(TypeEnv, Exp1, TempTypeEnv), !,
        typecheckExp(TempTypeEnv, Exp2, NewTypeEnv), !.
rawTypecheckBody(State, TypeEnv, body_setvar(VarName, Term), NewTypeEnv) :-
        !,
        typecheckVarUse(State, TypeEnv, VarName, Term, NewTypeEnv), !.
rawTypecheckBody(State, TypeEnv, body_getvar(VarName, Term), NewTypeEnv) :-
        !,
        typecheckVarUse(State, TypeEnv, VarName, Term, NewTypeEnv), !.
rawTypecheckBody(State, TypeEnv, bodyUnary(_, Body), NewTypeEnv) :-
        !,
        typecheckBody(State, TypeEnv, Body, NewTypeEnv), !.
rawTypecheckBody(State, TypeEnv, bodyPair(B1, _, B2), NewTypeEnv) :-
        !,
        typecheckBody(State, TypeEnv, B1, TempTypeEnv), !,
        typecheckBody(State, TempTypeEnv, B2, NewTypeEnv), !.
rawTypecheckBody(State, TypeEnv, higherOrderCall(What, ActualParams), NewTypeEnv) :-
        !,
        typeofTerm(State, TypeEnv, What, relationType(FormalParams), TempTypeEnv), !,
        typeofTerms(State, TempTypeEnv, ActualParams, FormalParams, NewTypeEnv), !.
rawTypecheckBody(State, TypeEnv, firstOrderCall(Name, ActualParams), NewTypeEnv) :-
        length(ActualParams, Arity),
        FormalParams = _, % introduce variable
        onFailure(
            lambda([], expectedFormalParamTypes(State, Name, Arity, FormalParams)),
            lambda([], yolo_UNSAFE_format_shim('Unknown clause: ~w~n~n', [pair(Name, Arity)]))),
        typeofTerms(State, TypeEnv, ActualParams, FormalParams, NewTypeEnv), !.

clausedef(typeofTerm, [], [state,
                           list(pair(int, type)), % input type environment
                           term, type,
                           list(pair(int, type))]). % output type environment
typeofTerm(State, TypeEnv, Term, ExpectedType, NewTypeEnv) :-
        onFailure(
            lambda([], rawTypeofTerm(State, TypeEnv, Term, ExpectedType, NewTypeEnv)),
            lambda([],
                   (% try to see what type it actually was
                    yolo_UNSAFE_format_shim('Type error at term ~w~n', [Term]),
                    onFailure(
                        lambda([],
                               (rawTypeofTerm(State, TypeEnv, Term, ActualType, _),
                                yolo_UNSAFE_format_shim('\tFound: ~w~n\tExpected: ~w~n~n', [ActualType, ExpectedType]))),
                        lambda([],
                               (yolo_UNSAFE_format_shim('\tFound: UNKNOWN~n\tExpected: ~w~n~n', [ExpectedType]))))))).

clausedef(rawTypeofTerm, [], [state,
                              list(pair(int, type)), % input type environment
                              term, type,
                              list(pair(int, type))]). % output type environment
rawTypeofTerm(_, TypeEnv, term_var(Variable), Type, NewTypeEnv) :-
        envVariableType(TypeEnv, Variable, Type, NewTypeEnv).
rawTypeofTerm(_, TypeEnv, term_num(_), intType, TypeEnv).
rawTypeofTerm(State, TypeEnv, term_lambda(Params, Body), relationType(Types), TypeEnv) :-
        !,
        typeofTerms(State, TypeEnv, Params, Types, TempTypeEnv),
        typecheckBody(State, TempTypeEnv, Body, _).
rawTypeofTerm(State, TypeEnv,
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
rawTypeofTerm(_, TypeEnv, term_constructor(_, []), atomType, TypeEnv).

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

clausedef(typecheckClauseWithErrorMessage, [], [state, clauseclause]).
typecheckClauseWithErrorMessage(State, Clause) :-
    onFailure(
            lambda([], typecheckClause(State, Clause)),
            lambda([], yolo_UNSAFE_format_shim('Type error at clause ~w~n~n', [Clause]))).

clausedef(typecheckClause, [], [state, clauseclause]).
typecheckClause(State, clauseclause(Name, FormalParams, Body)) :-
        length(FormalParams, Arity),
        expectedFormalParamTypes(State, Name, Arity, Generics, Expected),

        % Instantiate all the generics to type variables.  These exist only
        % while typechecking clauses.  They are in place so we don't try
        % to unify them with something that assumes we know something about
        % the type, as with:
        %
        % clausedef(test, [A], [A]).
        % test(1).
        %
        foldLeft(Generics, 0,
                 lambda([Accum, typevar(Accum), NewAccum],
                        NewAccum is Accum),
                 _),

        typeofTerms(State, [], FormalParams, Expected, TypeEnv),
        (markedUnsafe(Name) ->
            true;
            typecheckBody(State, TypeEnv, Body, _)),
        !.

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
        forall(Clauses, lambda([Clause], typecheckClauseWithErrorMessage(State, Clause))).
