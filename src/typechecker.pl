:- module('typechecker', [typecheckClauses/4]).

:- use_module('util.pl').

% -DataDef:     DataDef
% -Constructor: Alternative
% -Pair:        pair(Name, DataDef)
makeConstructorDataDefPair(DataDef, Alternative, pair(Name, DataDef)) :-
        Alternative =.. [Name|_].

% -DataDef: DataDef
% -CurMapping: [pair(Name, DataDef)]
% -ResMapping: [pair(Name, DataDef)]
%
% Assumes that sanitization has occurred.
addDataDefToMapping(DataDef, CurMapping, ResMapping) :-
        DataDef = datadef(_, _, Alternatives),
        maplist(makeConstructorDataDefPair(DataDef), Alternatives, NewPairs),
        append(NewPairs, CurMapping, ResMapping).

% -DataDefs:   [DataDef]
% -CurMapping: [pair(Name, DataDef)]
% -ResMapping: [pair(Name, DataDef)]
constructorToDataDefMapping([], Mapping, Mapping).
constructorToDataDefMapping([H|T], CurMapping, ResMapping) :-
        addDataDefToMapping(H, CurMapping, TempMapping),
        constructorToDataDefMapping(T, TempMapping, ResMapping).

% -DataDefs:             [DataDef]
% -ConstructorToDataDef: [pair(Name, DataDef)]
%
% Assumes that the data defs have been sanitized.
constructorToDataDefMapping(DataDefs, Mapping) :-
        constructorToDataDefMapping(DataDefs, [], Mapping).

% -ClauseDefs:                 [ClauseDef]
% -ClauseNameArityToClauseDef: [pair(pair(Name, Int), ClauseDef)]
%
% Assumes that the clause defs have been sanitized.
clauseNameArityToClauseDefMapping([], []).
clauseNameArityToClauseDefMapping([H|T], [pair(pair(Name, Arity), H)|Rest]) :-
        H = clausedef(Name, _, Params),
        length(Params, Arity),
        clauseNameArityToClauseDefMapping(T, Rest).

% -DataDefs:        [DataDef]
% -ConstructorName: Name
% -DataDef:         DataDef
%
% The returned datadef will have fresh variables.
getDataDef(DataDefs, ConstructorName, DataDef) :-
        member(pair(ConstructorName, RawDataDef), DataDefs),

        % introduces fresh type variables
        copy_term(RawDataDef, DataDef).

% -ClauseDefs:  [ClauseDef]
% -ClauseName:  Name
% -ClauseArity: Int
% -Expected:    [Type]
%
% Will introduce fresh type variables.
getClauseDefExpectedTypes(ClauseDefs, ClauseName, ClauseArity, Expected) :-
        member(pair(pair(ClauseName, ClauseArity), RawClauseDef), ClauseDefs),
        copy_term(RawClauseDef, clausedef(_, _, Expected)).

% -DataDefAlternatives: [Alternative]
% -ConstructorName:     Name
% -Alternative:         Alternative
dataDefAlternative([H|_], ConstructorName, H) :-
        H =.. [ConstructorName|_],
        !.
dataDefAlternative([_|T], ConstructorName, Alternative) :-
        dataDefAlternative(T, ConstructorName, Alternative),
        !.

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -GlobalVarDefs:    [GlobalVarDef]
% -TypeEnv:          [pair(Variable, Type)]
% -ConstructorName:  Name
% -ConstructorArgs:  [Term]
% -Type:             type([Type])
% -NewTypeEnv:       [pair(Variable, Type)]
constructorType(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
                ConstructorName, ConstructorArgs, Type, NewTypeEnv) :-
        getDataDef(DataDefs, ConstructorName,
                   datadef(TypeName, TypeParams, Alternatives)),
        dataDefAlternative(Alternatives, ConstructorName, Alternative),
        Alternative =.. [_|ExpectedTypes],
        typeofTerms(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
                    ConstructorArgs, ExpectedTypes, NewTypeEnv),
        Type =.. [TypeName|TypeParams].

% ---TypeEnv: [pair(Variable, Type)]---
% ONLY interact with type environment though `envVariableType`.
% Variables are uninstantiated, and we want to keep them that way.

% -TypeEnv:    [pair(Variable, Type)]
% -Variable:   Variable, should be uninstantiated
% -Type:       Type
% -NewTypeEnv: [pair(Variable, Type)]
%
% Gets the type of the variable in the type environment.  If the
% variable doesn't exist in the type environment, it will add it to
% the type environment.
envVariableType([], Variable, Type, [pair(Variable, Type)]) :- !.
envVariableType(Found, Variable, Type, Found) :-
        Found = [pair(EnvVariable, EnvType)|_],
        EnvVariable == Variable,
        !,
        EnvType = Type.
envVariableType([H|T], Variable, Type, [H|Rest]) :-
        envVariableType(T, Variable, Type, Rest).

% -Variable:   Variable, should be uninstantiated
% -Type:       Type
% -TypeEnv:    [pair(Variable, Type)]
% -NewTypeEnv: [pair(Variable, Type)]
envVariableType_(Variable, Type, TypeEnv, NewTypeEnv) :-
        envVariableType(TypeEnv, Variable, Type, NewTypeEnv).

% -TypeEnv:  [pair(Variable, Type)]
% -Variable: Variable
%
% Succeeds if the given type environment contains the given variable.
envContainsVar([pair(EnvVariable, _)|_], Variable) :-
        EnvVariable == Variable,
        !.
envContainsVar([_|T], Variable) :-
        envContainsVar(T, Variable).

% -AfterCallTypeEnv:  [pair(Variable, Type)]
% -BeforeCallTypeEnv: [pair(Variable, Type)]
% -ResultTypeEnv:     [pair(Variable, Type)]
%
% Will subtract any new variables added, but will not subtract
% additional type information learned.
afterBeforeCallTypeEnvs([], _, []) :- !.
afterBeforeCallTypeEnvs([H|T], Before, [H|Rest]) :-
        H = pair(Variable, _),
        envContainsVar(Before, Variable),
        !,
        afterBeforeCallTypeEnvs(T, Before, Rest).
afterBeforeCallTypeEnvs([_|T], Before, Rest) :-
        afterBeforeCallTypeEnvs(T, Before, Rest).

% ---DataDefMapping: [pair(Name, DataDef)]---
% Maps constructor names to their corresponding data defs
%
% ---ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% Maps first-order call names and arities with the corresponding definition.

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -GlobalVarDefs:    [GlobalVarDef]
% -TypeEnv:          [pair(Variable, Type)]
% -Terms:            [Term]
% -Types:            [Type]
% -NewTypeEnv:       [pair(Variable, Type)]
%
% Only succeeds if the terms and types are of the same length
typeofTerms(_, _, _, TypeEnv, [], [], TypeEnv).
typeofTerms(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
            [HTerm|Terms], [HType|Types], NewTypeEnv) :-
        typeofTerm(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
                   HTerm, HType, TempTypeEnv),
        typeofTerms(DataDefs, ClauseDefs, GlobalVarDefs, TempTypeEnv,
                    Terms, Types, NewTypeEnv).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -GlobalVarDefs:    [GlobalVarDef]
% -TypeEnv:          [pair(Variable, Type)]
% -Term:             Term
% -Type:             Type
% -NewTypeEnv:       [pair(Variable, Type)]
typeofTerm(_, _, _, TypeEnv, X, Type, NewTypeEnv) :-
        var(X),
        !,
        envVariableType(TypeEnv, X, Type, NewTypeEnv).
typeofTerm(DataDefs, ClauseDefs, GlobalVarDefs,
           TypeEnv, Atom, Type, NewTypeEnv) :-
        atom(Atom),
        !,
        (constructorType(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
                         Atom, [], Type, NewTypeEnv) ->
                         true;
                         (TypeEnv = NewTypeEnv,
                          Type = atom)).
typeofTerm(_, _, _, TypeEnv, N, int, TypeEnv) :-
        number(N),
        !.
typeofTerm(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
           lambda(Params, Body), relation(Types), NewTypeEnv) :-
        !,
        typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs,
                      TypeEnv, Body, TempTypeEnv),
        typeofTerms(DataDefs, ClauseDefs, GlobalVarDefs, TempTypeEnv,
                    Params, Types, PostLambdaTypeEnv),

        % any variable introduced in the body of the lambda do not live
        % beyond the lambda.  However, we cannot just discard the type
        % environment altogether, as within the body we may have learned
        % more information about captured variables.
        afterBeforeCallTypeEnvs(PostLambdaTypeEnv, TypeEnv, NewTypeEnv).

typeofTerm(DataDefs, ClauseDefs, GlobalVarDefs,
           TypeEnv, Structure, Type, NewTypeEnv) :-
        Structure =.. [ConstructorName|Params],
        !,
        constructorType(DataDefs, ClauseDefs, GlobalVarDefs,
                        TypeEnv, ConstructorName,
                        Params, Type, NewTypeEnv).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -GlobalVarDefs:    [GlobalVarDef]
% -TypeEnv:          [pair(Variable, Type)]
% -Body:             Body
% -NewTypeEnv:       [pair(Variable, Type)]
%
% Since bodies don't return anything, there is no associated return type.
typecheckBody(_, _, _, TypeEnv, AtomForm, TypeEnv) :-
        bodyAtomForm(AtomForm),
        !.
typecheckBody(_, _, _, TypeEnv, is(VarOrNum, ArithExp), NewTypeEnv) :-
        !,
        term_variables(VarOrNum, Variables, Temp),
        term_variables(ArithExp, Temp),
        length(Variables, NumVariables),
        length(IntTypes, NumVariables),
        maplist(=(int), IntTypes),
        foldl(envVariableType_, Variables, IntTypes, TypeEnv, NewTypeEnv).
typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
              BodyVarForm, NewTypeEnv) :-
        bodyVarForm(BodyVarForm, Name, Term),
        !,
        % We intentionally don't freshen type variables here.  If we did, then
        % each time we accessed the variable, we would be allowed to change the
        % type, which would be unsound.  Instead, we leave original type variables
        % in, and allow one instantiation in the program.  I'm honestly not sure
        % how useful this is because only one instantiation is ever possible with
        % this, but it's sound anyway.
        member(globalvardef(Name, _, ExpectedType), GlobalVarDefs),

        typeofTerm(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
                   Term, ExpectedType, NewTypeEnv).
typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs,
              TypeEnv, PairForm, NewTypeEnv) :-
        bodyPairForm(PairForm, B1, B2),
        !,
        typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs,
                      TypeEnv, B1, TempTypeEnv),
        typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs,
                      TempTypeEnv, B2, NewTypeEnv).
typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
              HigherOrderCall, NewTypeEnv) :-
        HigherOrderCall =.. [call, Relation|Params],
        !,
        RelationType = relation(ExpectedTypes),
        typeofTerm(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv, Relation,
                   RelationType, TempTypeEnv),
        typeofTerms(DataDefs, ClauseDefs, GlobalVarDefs, TempTypeEnv,
                    Params, ExpectedTypes, NewTypeEnv).
typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs,
              TypeEnv, FirstOrderCall, NewTypeEnv) :-
        FirstOrderCall =.. [Name|Params],
        !,
        length(Params, Arity),
        getClauseDefExpectedTypes(ClauseDefs, Name, Arity, ExpectedTypes),
        typeofTerms(DataDefs, ClauseDefs, GlobalVarDefs, TypeEnv,
                    Params, ExpectedTypes, NewTypeEnv).

% -Name
%
% Succeeds if the given clause name is marked unsafe
markedUnsafe(Name) :-
        UnsafeMarker = 'yolo_UNSAFE_',
        atom_codes(Name, NameList),
        atom_codes(UnsafeMarker, UnsafeList),
        listContainsList(NameList, UnsafeList).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -GlobalVarDefs:    [GlobalVarDef]
% -Clause:           Clause
typecheckClause(DataDefs, ClauseDefs, GlobalVarDefs, :-(Head, Body)) :-
        Head =.. [Name|Params],
        length(Params, Arity),
        getClauseDefExpectedTypes(ClauseDefs, Name, Arity, Expected),
        typeofTerms(DataDefs, ClauseDefs, GlobalVarDefs, [],
                    Params, Expected, TypeEnv),
        (markedUnsafe(Name) ->
            true;
            typecheckBody(DataDefs, ClauseDefs, GlobalVarDefs,
                          TypeEnv, Body, _)).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -GlobalVarDefs:    [GlobalVarDef]
% -Clauses:          [Clause]
typecheckClausesWithMappings(DataDefs, ClauseDefs, GlobalVarDefs, Clauses) :-
        maplist(typecheckClause(DataDefs, ClauseDefs, GlobalVarDefs), Clauses).

builtinDataDef(datadef(list, [A], [.(A, list(A)), []])).

builtinDataDefs(DataDefs) :-
        findall(D, builtinDataDef(D), DataDefs).

builtinClauseDef(clausedef(>, [], [int, int])).
builtinClauseDef(clausedef(<, [], [int, int])).
builtinClauseDef(clausedef(=<, [], [int, int])).
builtinClauseDef(clausedef(>=, [], [int, int])).
builtinClauseDef(clausedef(=, [A], [A, A])).
builtinClauseDef(clausedef(==, [A], [A, A])).

builtinClauseDefs(ClauseDefs) :-
        findall(C, builtinClauseDef(C), ClauseDefs).

% -UserDataDefs:      [DataDef]
% -UserClauseDefs:    [ClauseDef]
% -UserGlobalVarDefs: [GlobalVarDef]
% -UserClauses:       [NormalizedClause]
%
% Will add in builtins itself.
typecheckClauses(UserDataDefs, UserClauseDefs, GlobalVarDefs, Clauses) :-
        % add in builtins
        builtinDataDefs(BuiltinDataDefs),
        builtinClauseDefs(BuiltinClauseDefs),
        append(BuiltinDataDefs, UserDataDefs, DataDefs),
        append(BuiltinClauseDefs, UserClauseDefs, ClauseDefs),

        % perform typechecking
        constructorToDataDefMapping(DataDefs, DataDefMapping),
        clauseNameArityToClauseDefMapping(ClauseDefs, ClauseDefMapping),
        typecheckClausesWithMappings(DataDefMapping, ClauseDefMapping,
                                     GlobalVarDefs, Clauses).
