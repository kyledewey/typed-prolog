:- use_module('sanitizer.pl').

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
        member(pair(pair(ClauseName, ClauseArity),
                    clausedef(_, _, Expected)), ClauseDefs).

% -DataDefAlternatives: [Alternative]
% -ConstructorName:     Name
% -Alternative:         Alternative
dataDefAlternative([H|_], ConstructorName, H) :-
        H =.. [ConstructorName|_],
        !.
dataDefAlternative([_|T], ConstructorName, Alternative) :-
        dataDefAlternative(T, ConstructorName, Alternative),
        !.

% -List1: [A]
% -List2: [B]
sameLength([], []).
sameLength([_|T1], [_|T2]) :-
        sameLength(T1, T2).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -TypeEnv:          [pair(Variable, Type)]
% -ConstructorName:  Name
% -ConstructorArgs:  [Term]
% -Type:             type([Type])
% -NewTypeEnv:       [pair(Variable, Type)]
constructorType(DataDefs, ClauseDefs, TypeEnv, ConstructorName,
                ConstructorArgs, Type, NewTypeEnv) :-
        getDataDef(DataDefs, ConstructorName,
                   datadef(TypeName, TypeParams, Alternatives)),
        dataDefAlternative(Alternatives, ConstructorName, Alternative),
        Alternative =.. [_|ExpectedTypes],
        typeofTerms(DataDefs, ClauseDefs, TypeEnv,
                    ConstructorArgs, ExpectedTypes, NewTypeEnv),
        Type =.. [TypeName|TypeParams].

% ---TypeEnv: [pair(Variable, Type)]---
% ONLY interact with type environment though envVariableType.
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
        Found = [pair(EnvVariable, Type)|_],
        EnvVariable == Variable,
        !.
envVariableType([H|T], Variable, Type, [H|Rest]) :-
        envVariableType(T, Variable, Type, Rest).

% ---DataDefMapping: [pair(Name, DataDef)]---
% Maps constructor names to their corresponding data defs
%
% ---ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% Maps first-order call names and arities with the corresponding definition.

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -TypeEnv:          [pair(Variable, Type)]
% -Terms:            [Term]
% -Types:            [Type]
% -NewTypeEnv:       [pair(Variable, Type)]
%
% Only succeeds if the terms and types are of the same length
typeofTerms(_, _, TypeEnv, [], [], TypeEnv).
typeofTerms(DataDefs, ClauseDefs, TypeEnv,
            [HTerm|Terms], [HType|Types], NewTypeEnv) :-
        typeofTerm(DataDefs, ClauseDefs, TypeEnv,
                   HTerm, HType, TempTypeEnv),
        typeofTerms(DataDefs, ClauseDefs, TempTypeEnv,
                    Terms, Types, NewTypeEnv).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -TypeEnv:          [pair(Variable, Type)]
% -Term:             Term
% -Type:             Type
% -NewTypeEnv:       [pair(Variable, Type)]
typeofTerm(_, _, TypeEnv, X, Type, NewTypeEnv) :-
        var(X),
        !,
        envVariableType(TypeEnv, X, Type, NewTypeEnv).
typeofTerm(DataDefs, ClauseDefs, TypeEnv, Atom, Type, NewTypeEnv) :-
        atom(Atom),
        !,
        constructorType(DataDefs, ClauseDefs, TypeEnv,
                        Atom, [], Type, NewTypeEnv).
typeofTerm(_, _, TypeEnv, N, int, TypeEnv) :-
        number(N),
        !.
typeofTerm(DataDefs, ClauseDefs, TypeEnv, Lambda, Relation, NewTypeEnv) :-
        Lambda =.. [lambda, Params, Body],
        !,
        typecheckBody(DataDefs, ClauseDefs, TypeEnv, Body, TempTypeEnv),
        typeofTerms(DataDefs, ClauseDefs, TempTypeEnv,
                    Params, Types, NewTypeEnv),
        Relation =.. [relation|Types].
typeofTerm(DataDefs, ClauseDefs, TypeEnv, Structure, Type, NewTypeEnv) :-
        Structure =.. [ConstructorName|Params],
        !,
        constructorType(DataDefs, ClauseDefs, TypeEnv, ConstructorName,
                        Params, Type, NewTypeEnv).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -TypeEnv:          [pair(Variable, Type)]
% -Body:             Body
% -NewTypeEnv:       [pair(Variable, Type)]
%
% Since bodies don't return anything, there is no associated return type.
typecheckBody(_, _, TypeEnv, AtomForm, TypeEnv) :-
        bodyAtomForm(AtomForm),
        !.
typecheckBody(DataDefs, ClauseDefs, TypeEnv, PairForm, NewTypeEnv) :-
        bodyPairForm(PairForm, B1, B2),
        !,
        typecheckBody(DataDefs, ClauseDefs, TypeEnv, B1, TempTypeEnv),
        typecheckBody(DataDefs, ClauseDefs, TempTypeEnv, B2, NewTypeEnv).
typecheckBody(DataDefs, ClauseDefs, TypeEnv, =(T1, T2), NewTypeEnv) :-
        !,
        typeofTerm(DataDefs, ClauseDefs, TypeEnv, T1, Type, TempTypeEnv),
        typeofTerm(DataDefs, ClauseDefs, TempTypeEnv, T2, Type, NewTypeEnv).
typecheckBody(DataDefs, ClauseDefs, TypeEnv, HigherOrderCall, NewTypeEnv) :-
        HigherOrderCall =.. [call, Relation|Params],
        !,
        typeofTerm(DataDefs, ClauseDefs, TypeEnv, Relation,
                   RelationType, TempTypeEnv),
        RelationType =.. [relation|ExpectedTypes],
        typeofTerms(DataDefs, ClauseDefs, TempTypeEnv,
                    Params, ExpectedTypes, NewTypeEnv).
typecheckBody(DataDefs, ClauseDefs, TypeEnv, FirstOrderCall, NewTypeEnv) :-
        FirstOrderCall =.. [Name|Params],
        !,
        length(Params, Arity),
        getClauseDefExpectedTypes(ClauseDefs, Name, Arity, ExpectedTypes),
        typeofTerms(DataDefs, ClauseDefs, TypeEnv,
                    Params, ExpectedTypes, NewTypeEnv).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -Clause:           Clause
typecheckClause(DataDefs, ClauseDefs, RawClause) :-
        copy_term(RawClause, :-(Head, Body)),
        Head =.. [Name|Params],
        length(Params, Arity),
        getClauseDefExpectedTypes(ClauseDefs, Name, Arity, Expected),
        typeofTerms(DataDefs, ClauseDefs, [], Params, Expected, TypeEnv),
        typecheckBody(DataDefs, ClauseDefs, TypeEnv, Body, _).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -Clauses:          [Clause]
typecheckClauses(DataDefs, ClauseDefs, Clauses) :-
        maplist(typecheckClause(DataDefs, ClauseDefs), Clauses).

% -Stream:  Stream
% -Clauses: [Clause]
clausesInStream(Stream, Clauses) :-
        read_clause(Stream, Clause, []),
        ((Clause == end_of_file) ->
            (Clauses = []);
            (Clauses = [Clause|Rest],
             clausesInStream(Stream, Rest))).

% -Filename
% -Clauses: [Clause]
clausesInFile(Filename, Clauses) :-
        open(Filename, read, Stream, []),
        clausesInStream(Stream, Clauses),
        close(Stream).

isDataDef(datadef(_, _, _)).
isClauseDef(clausedef(_, _, _)).

% -InputClause:  Clause
% -OutputClause: Head :- Body.
%
% Read in clauses will either be a head or a full horn clause.
% This will make everything a horn clause.
normalizeClause(:-(Head, Body), :-(Head, Body)) :- !.
normalizeClause(Clause, :-(Clause, true)).

% -Filename
typecheckFile(Filename) :-
        clausesInFile(Filename, Clauses1),

        % extract out into data definitions, clause definitions, and everything
        % else.
        partition(isDataDef, Clauses1, RawDataDefs, Clauses2),
        partition(isClauseDef, Clauses2, RawClauseDefs, Clauses3),

        % sanitize them
        maplist(normalizeClause, Clauses3, NormalizedClauses),
        ensureProgram(RawDataDefs, RawClauseDefs, NormalizedClauses),

        % do typechecking
        constructorToDataDefMapping(RawDataDefs, DataDefMapping),
        clauseNameArityToClauseDefMapping(RawClauseDefs, ClauseDefMapping),
        typecheckClauses(DataDefMapping, ClauseDefMapping, NormalizedClauses).
