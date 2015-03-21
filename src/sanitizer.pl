:- module('sanitizer', [sanitizeFile/1]).

:- use_module('util.pl').

% The syntax we consider is more restrictive than everything in Prolog.
% Most importantly, we have a distinction between code and data, with
% lambdas being a well-defined intermediary between the two.  In this module,
% we make sure that things are syntactically well-formed.

% -Terms:              [Term]
ensureTerms(Terms) :-
        (var(Terms) ->
            fail;
            maplist(ensureTerm, Terms)).

% -Term:               Term
ensureTerm(Var) :-
        var(Var),
        !.
ensureTerm(Atom) :-
        atom(Atom),
        !.
ensureTerm(Int) :-
        number(Int),
        !.
ensureTerm(lambda(Params, Body)) :-
        !,
        ensureTerms(Params),
        ensureBody(Body).
ensureTerm(Structure) :-
        Contents = [_|_],
        Structure =.. [_|Contents],
        !,
        ensureTerms(Contents).

%    e \in Exp ::= n | x | e_1 op e_2
% op \in Binop ::= + | - | * | / | min | max
ensureArithExp(N) :-
        number(N),
        !.
ensureArithExp(X) :-
        var(X),
        !.
ensureArithExp(Binop) :-
        Binop =.. [Op, E1, E2],
        atom(Op),
        member(Op, ['+', '-', '*', '/', 'min', 'max']),
        ensureArithExp(E1),
        ensureArithExp(E2).

% -Body: Body
ensureBody(Var) :-
        var(Var),
        !,
        fail.
ensureBody(AtomForm) :-
        bodyAtomForm(AtomForm),
        !.
ensureBody(is(VarOrNum, Exp)) :-
        !,
        (var(VarOrNum); number(VarOrNum)),
        ensureArithExp(Exp).
ensureBody(VarForm) :-
        bodyVarForm(VarForm, VarName, Term),
        !,
        atom(VarName),
        ensureTerm(Term).
ensureBody(PairForm) :-
        bodyPairForm(PairForm, B1, B2),
        !,
        ensureBody(B1),
        ensureBody(B2).
ensureBody(HigherOrderCall) :-
        HigherOrderCall =.. [call|Params],
        !,
        ensureTerms(Params).
ensureBody(FirstOrderCall) :-
        FirstOrderCall =.. [Name|Params],
        atom(Name),
        !,
        ensureTerms(Params).

% -TypeVarsInScope:   [TypeVar]
% -Type
ensureType_(TypeVarsInScope, Type) :-
        ensureType(Type, TypeVarsInScope).

% -Types:             [Type]
% -TypeVarsInScope:   [TypeVar]
ensureTypes(Types, TypeVarsInScope) :-
        maplist(ensureType_(TypeVarsInScope), Types).

% -Type
% -TypeVarsInScope:   [TypeVar]
ensureType(TypeVar, TypeVarsInScope) :-
        var(TypeVar),
        !,
        memberEqual(TypeVar, TypeVarsInScope).
ensureType(int, _) :- !.
ensureType(atom, _) :- !.
ensureType(relation(Types), TypeVarsInScope) :-
        !,
        ensureTypes(Types, TypeVarsInScope).
ensureType(ConstructorType, TypeVarsInScope) :-
        ConstructorType =.. [_|Types],
        !,
        ensureTypes(Types, TypeVarsInScope).

% -TypeVarsInScope:   [TypeVar]
% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -Alternative
ensureDataDefAlternative(TypeVarsInScope,
                         SeenConstructors, [AltName|SeenConstructors],
                         Alternative) :-
        Alternative =.. [AltName|Types],
        \+ memberEqual(AltName, SeenConstructors),
        ensureTypes(Types, TypeVarsInScope).

% -TypeVarsInScope:   [TypeVar]
% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -Alternatives:      [Alternative]
ensureDataDefAlternatives(_, Constructors, Constructors, []).
ensureDataDefAlternatives(TypeVarsInScope,
                          SeenConstructors, NewConstructors,
                          [H|T]) :-
        ensureDataDefAlternative(TypeVarsInScope,
                                 SeenConstructors, TempConstructors, H),
        ensureDataDefAlternatives(TypeVarsInScope,
                                  TempConstructors, NewConstructors, T).

% -TypeVars: [TypeVar]
ensureTypeVars(TypeVars) :-
        is_set(TypeVars),
        maplist(var, TypeVars).

% datadef(list, [A], [cons(A, list(A)), nil]).

% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -DataDef
ensureDataDef(SeenConstructors, NewConstructors,
              datadef(Name, TypeVarsInScope, Alternatives)) :-
        atom(Name),
        ensureTypeVars(TypeVarsInScope),
        Alternatives = [_|_], % non-empty
        ensureDataDefAlternatives(TypeVarsInScope,
                                  SeenConstructors, NewConstructors,
                                  Alternatives).

% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -DataDefs:          [DataDef]
ensureDataDefs(Constructors, Constructors, []).
ensureDataDefs(SeenConstructors, NewConstructors, [H|T]) :-
        ensureDataDef(SeenConstructors,
                      TempConstructors, H),
        ensureDataDefs(TempConstructors,
                       NewConstructors, T).
        
% -DataDefs: [DataDef]
ensureDataDefs(DataDefs) :-
        ensureDataDefs([], _, DataDefs).

% clausedef(map, [A], [list(A), relation([A, B]), list(B)])

% -ClauseDef
ensureClauseDef(clausedef(Name, TypeVars, ParamTypes)) :-
        atom(Name),
        ensureTypeVars(TypeVars),
        ensureTypes(ParamTypes, TypeVars).


% -ClauseDefs:        [ClauseDef]
ensureClauseDefs(ClauseDefs) :-
        maplist(ensureClauseDef, ClauseDefs).

% -Clause:             Clause
ensureClause(:-(Head, Body)) :-
        Head =.. [Name|Params],
        atom(Name),
        ensureTerms(Params),
        ensureBody(Body).

% -Clauses: [Clause]
ensureClauses(Clauses) :-
        maplist(ensureClause, Clauses).

% -GlobalVarDef:      GlobalVarDef
% -SeenGlobal:        [Name]
% -NewSeenGlobal:     [Name]
ensureGlobalVarDef(globalvardef(Name, TypeVars, Type),
                   Seen, [Name|Seen]) :-
        atom(Name),
        \+ member(Name, Seen),
        ensureTypeVars(TypeVars),
        ensureType(Type, TypeVars).

% -GlobalVarDefs:     [GlobalVarDef]
% -SeenGlobal:        [Name]
ensureGlobalVarDefs([], _).
ensureGlobalVarDefs([H|T], Seen) :-
        ensureGlobalVarDef(H, Seen, NewSeen),
        ensureGlobalVarDefs(T, NewSeen).

% -GlobalVarDefs: [GlobalVarDef]
ensureGlobalVarDefs(VarDefs) :-
        ensureGlobalVarDefs(VarDefs, []).

% -Name: Name
% -Arity: Int
% -Clause: Clause
clauseHasNameArity(Name, Arity, :-(Head, _)) :-
        Head =.. [Name|Params],
        length(Params, Arity).

clauseDefHasNameArity(Name, Arity, clausedef(Name, _, Params)) :-
        length(Params, Arity).

% -Clauses:   [Clause]
% -ClauseDef: ClauseDef
clauseDefInhabited(Clauses, clausedef(Name, _, Params)) :-
        length(Params, Arity),
        member(TestClause, Clauses),
        once(clauseHasNameArity(Name, Arity, TestClause)).

% -ClauseDefs: [ClauseDef]
% -Clause: Clause
clauseHasClauseDef(ClauseDefs, :-(Head, _)) :-
        Head =.. [Name|Params],
        length(Params, Arity),
        member(TestClauseDef, ClauseDefs),
        once(clauseDefHasNameArity(Name, Arity, TestClauseDef)).

% -Clauses:    [Clause]
% -ClauseDefs: [ClauseDef]
clauseDefsInhabited(Clauses, ClauseDefs) :-
        maplist(clauseDefInhabited(Clauses), ClauseDefs).

% -Clauses:    [Clause]
% -ClauseDefs: [ClauseDef]
clausesHaveClauseDef(Clauses, ClauseDefs) :-
        maplist(clauseHasClauseDef(ClauseDefs), Clauses).

% -UseModule:     UseModule
% -ClausesInput:  [/(Name, Arity)]
% -DataInput:     [Name]
% -ClausesOutput: [/(Name, Arity)]
% -DataOutput:    [Name]
moduleUseClausesData(use_module(_, ImportedClauses, ImportedData),
                     ClausesInput, DataInput,
                     ClausesOutput, DataOutput) :-
        appendDiffList(ImportedClauses, ClausesInput, ClausesOutput),
        appendDiffList(ImportedData, DataInput, DataOutput).

% -UseModules:    [UseModule]
% -ClausesInput:  [/(Name, Arity)]
% -DataInput:     [Name]
% -ClausesOutput: [/(Name, Arity)]
% -DataOutput:    [Name]
moduleUsesClausesData([], Clauses, Data, Clauses, Data).
moduleUsesClausesData([H|T], ClausesInput, DataInput, ClausesOutput, DataOutput) :-
        moduleUseClausesData(H, ClausesInput, DataInput, TempClauses, TempData),
        moduleUsesClausesData(T, TempClauses, TempData, ClausesOutput, DataOutput).

% -UseModules: [UseModule]
% -Clauses:    [/(Name, Arity)]
% -Data:       [Name]
moduleUsesClausesData(UseModules, Clauses, Data) :-
        moduleUsesClausesData(UseModules, Clauses, Data, [], []).

% -LoadedFile: loadedFile (see clauses_util.pl)
sanitizeFile(
        loadedFile(DataDefs, ClauseDefs, GlobalVarDefs,
                   module(_, ExportedClauses, ExportedData),
                   ModuleUses, Clauses)) :-
        % basic syntactic well-formedness
        ensureDataDefs(DataDefs), !,
        ensureClauseDefs(ClauseDefs), !,
        ensureGlobalVarDefs(GlobalVarDefs), !,
        ensureClauses(Clauses), !,

        % definitions have matching uses and vice-versa
        clauseDefsInhabited(Clauses, ClauseDefs), !,
        clausesHaveClauseDef(Clauses, ClauseDefs), !,

        % no duplicate exports exist
        is_set(ExportedClauses),
        is_set(ExportedData),

        % no duplicate imports exist
        moduleUsesClausesData(ModuleUses, ImportedClauses, ImportedData),
        is_set(ImportedClauses),
        is_set(ImportedData).
