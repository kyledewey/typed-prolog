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

% -List1: [A]
% -List2: [B]
sameLength([], []).
sameLength([_|T1], [_|T2]) :-
        sameLength(T1, T2).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -ConstructorName:  Name
% -ConstructorArgs:  [Term]
% -Type:             type([Type])
constructorType(DataDefs, ClauseDefs, ConstructorName,
                ConstructorArgs, Type) :-
        getDataDef(DataDefs, ConstructorName,
                   datadef(TypeName, TypeParams, Alternatives)),
        dataDefAlternative(Alternatives, ConstructorName, Alternative),
        Alternative =.. [_|ExpectedTypes],
        typeofTerms(DataDefs, ClauseDefs, ConstructorArgs, ExpectedTypes),
        Type =.. [TypeName|TypeParams].

% ---DataDefMapping: [pair(Name, DataDef)]---
% Maps constructor names to their corresponding data defs
%
% ---ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% Maps first-order call names and arities with the corresponding definition.

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -Terms:            [Term]
% -Types:            [Type]
typeofTerms(DataDefs, ClauseDefs, Terms, Types) :-
        sameLength(Terms, Types),
        maplist(typeofTerm(DataDefs, ClauseDefs), Terms, Types).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -Term:             Term
% -Type:             Type
typeofTerm(_, _, X, Type) :-
        var(X),
        !,
        % bind variables directly to their types.  The reason for
        % delaying this unification is that we want to explicitly
        % check first that X is uninstantiated.  In the event that
        % Type is instantiated, then the var check would fail.
        X = Type.
typeofTerm(DataDefs, ClauseDefs, Atom, Type) :-
        atom(Atom),
        !,
        constructorType(DataDefs, ClauseDefs, Atom, [], Type).
typeofTerm(_, _, N, int) :-
        number(N),
        !.
typeofTerm(DataDefs, ClauseDefs, Lambda, Relation) :-
        Lambda =.. [lambda, Params, Body],
        !,
        typecheckBody(DataDefs, ClauseDefs, Body),
        maplist(typeofTerm(DataDefs, ClauseDefs), Params, Types),
        Relation =.. [relation|Types].
typeofTerm(DataDefs, ClauseDefs, Structure, Type) :-
        Structure =.. [ConstructorName|Params],
        !,
        constructorType(DataDefs, ClauseDefs, ConstructorName,
                        Params, Type).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -Body:             Body
%
% Since bodies don't return anything, there is no return type.
typecheckBody(_, _, AtomForm) :-
        bodyAtomForm(AtomForm),
        !.
typecheckBody(DataDefs, ClauseDefs, PairForm) :-
        bodyPairForm(PairForm, B1, B2),
        !,
        typecheckBody(DataDefs, ClauseDefs, B1),
        typecheckBody(DataDefs, ClauseDefs, B2).
typecheckBody(DataDefs, ClauseDefs, =(T1, T2)) :-
        !,
        typeofTerm(DataDefs, ClauseDefs, T1, Type),
        typeofTerm(DataDefs, ClauseDefs, T2, Type).
typecheckBody(DataDefs, ClauseDefs, HigherOrderCall) :-
        HigherOrderCall =.. [call, Relation|Params],
        !,
        typeofTerm(DataDefs, ClauseDefs, Relation, RelationType),
        RelationType =.. [relation|ExpectedTypes],
        typeofTerms(DataDefs, ClauseDefs, Params, ExpectedTypes).
typecheckBody(DataDefs, ClauseDefs, FirstOrderCall) :-
        FirstOrderCall =.. [Name|Params],
        !,
        length(Params, Arity),
        getClauseDefExpectedTypes(ClauseDefs, Name, Arity, ExpectedTypes),
        typeofTerms(DataDefs, ClauseDefs, Params, ExpectedTypes).

% -DataDefMapping:   [pair(Name, DataDef)]
% -ClauseDefMapping: [pair(pair(Name, Int), ClauseDef)]
% -Clause:           Clause
typecheckClause(DataDefs, ClauseDefs, RawClause) :-
        copy_term(RawClause, :-(Head, Body)),
        Head =.. [Name|Params],
        length(Params, Arity),
        getClauseDefExpectedTypes(ClauseDefs, Name, Arity, Expected),
        typeofTerms(DataDefs, ClauseDefs, Params, Expected),
        typecheckBody(DataDefs, ClauseDefs, Body).

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
