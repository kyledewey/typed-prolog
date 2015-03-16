:- module('sanitizer', [ensureProgram/4]).

:- use_module('util.pl').

% The syntax we consider is more restrictive than everything in Prolog.
% Most importantly, we have a distinction between code and data, with
% lambdas being a well-defined intermediary between the two.  In this module,
% we make sure that things are syntactically well-formed.  We also check
% that at the very least, we have calls with the appropriate name/arity,
% which we can check cheaply here with information we need to gather anyway.

% -ClauseDefNameArity: [pair(Name, Arity)]
% -Terms:              [Term]
ensureTerms(ClauseDefNameArity, Terms) :-
        maplist(ensureTerm(ClauseDefNameArity), Terms).

% -ClauseDefNameArity: [pair(Name, Arity)]
% -Term:               Term
ensureTerm(_, Var) :-
        var(Var),
        !.
ensureTerm(_, Atom) :-
        atom(Atom),
        !.
ensureTerm(_, Int) :-
        number(Int),
        !.
ensureTerm(ClauseDefNameArity, lambda(Params, Body)) :-
        !,
        ensureTerms(ClauseDefNameArity, Params),
        ensureBody(ClauseDefNameArity, Body).
ensureTerm(ClauseDefNameArity, Structure) :-
        Contents = [_|_],
        Structure =.. [_|Contents],
        !,
        ensureTerms(ClauseDefNameArity, Contents).

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

% -ClauseDefNameArity: [pair(Name, Arity)]
% -Body: Body
ensureBody(_, AtomForm) :-
        bodyAtomForm(AtomForm),
        !.
ensureBody(_, is(VarOrNum, Exp)) :-
        !,
        (var(VarOrNum); number(VarOrNum)),
        ensureArithExp(Exp).
ensureBody(ClauseDefNameArity, VarForm) :-
        bodyVarForm(VarForm, VarName, Term),
        !,
        atom(VarName),
        ensureTerm(ClauseDefNameArity, Term).
ensureBody(ClauseDefNameArity, PairForm) :-
        bodyPairForm(PairForm, B1, B2),
        !,
        ensureBody(ClauseDefNameArity, B1),
        ensureBody(ClauseDefNameArity, B2).
ensureBody(ClauseDefNameArity, HigherOrderCall) :-
        HigherOrderCall =.. [call|Params],
        !,
        ensureTerms(ClauseDefNameArity, Params).
ensureBody(ClauseDefNameArity, FirstOrderCall) :-
        FirstOrderCall =.. [Name|Params],
        atom(Name),
        !,
        length(Params, Arity),
        member(pair(Name, Arity), ClauseDefNameArity),
        ensureTerms(ClauseDefNameArity, Params).

% -TypeVarsInScope:   [TypeVar]
% -DataDefNamesArity: [pair(Name, Arity)]
% -Type
ensureType_(TypeVarsInScope, DataDefNamesArity, Type) :-
        ensureType(Type, TypeVarsInScope, DataDefNamesArity).

% -Types:             [Type]
% -TypeVarsInScope:   [TypeVar]
% -DataDefNamesArity: [pair(Name, Arity)]
ensureTypes(Types, TypeVarsInScope, DataDefNamesArity) :-
        maplist(ensureType_(TypeVarsInScope, DataDefNamesArity), Types).

% -Type
% -TypeVarsInScope:   [TypeVar]
% -DataDefNamesArity: [pair(Name, Arity)]
ensureType(TypeVar, TypeVarsInScope, _) :-
        var(TypeVar),
        !,
        memberEqual(TypeVar, TypeVarsInScope).
ensureType(int, _, _) :- !.
ensureType(relation(Types), TypeVarsInScope, DataDefNamesArity) :-
        !,
        ensureTypes(Types, TypeVarsInScope, DataDefNamesArity).
ensureType(ConstructorType, TypeVarsInScope, DataDefNamesArity) :-
        ConstructorType =.. [Name|Types],
        !,
        length(Types, Arity),
        member(pair(Name, Arity), DataDefNamesArity),
        ensureTypes(Types, TypeVarsInScope, DataDefNamesArity).

% -TypeVarsInScope:   [TypeVar]
% -DataDefNamesArity: [pair(Name, Arity)]
% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -Alternative
ensureDataDefAlternative(TypeVarsInScope, DataDefNamesArity,
                         SeenConstructors, [AltName|SeenConstructors],
                         Alternative) :-
        Alternative =.. [AltName|Types],
        \+ memberEqual(AltName, SeenConstructors),
        ensureTypes(Types, TypeVarsInScope, DataDefNamesArity).

% -TypeVarsInScope:   [TypeVar]
% -DataDefNamesArity: [pair(Name, Arity)]
% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -Alternatives:      [Alternative]
ensureDataDefAlternatives(_, _, Constructors, Constructors, []).
ensureDataDefAlternatives(TypeVarsInScope, DataDefNamesArity,
                          SeenConstructors, NewConstructors,
                          [H|T]) :-
        ensureDataDefAlternative(TypeVarsInScope, DataDefNamesArity,
                                 SeenConstructors, TempConstructors, H),
        ensureDataDefAlternatives(TypeVarsInScope, DataDefNamesArity,
                                  TempConstructors, NewConstructors, T).

% -TypeVars: [TypeVar]
ensureTypeVars(TypeVars) :-
        is_set(TypeVars),
        maplist(var, TypeVars).

% datadef(list, [A], [cons(A, list(A)), nil]).

% -DataDefNamesArity: [pair(Name, Arity)]
% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -DataDef
ensureDataDef(DataDefNamesArity, SeenConstructors, NewConstructors,
              datadef(Name, TypeVarsInScope, Alternatives)) :-
        atom(Name),
        ensureTypeVars(TypeVarsInScope),
        Alternatives = [_|_], % non-empty
        ensureDataDefAlternatives(TypeVarsInScope, DataDefNamesArity,
                                  SeenConstructors, NewConstructors,
                                  Alternatives).

% -DataDefNamesArity: [pair(Name, Arity)]
% -SeenConstructors:  [Name]
% -NewConstructors:   [Name]
% -DataDefs:          [DataDef]
ensureDataDefs(_, Constructors, Constructors, []).
ensureDataDefs(DataDefNamesArity, SeenConstructors, NewConstructors, [H|T]) :-
        ensureDataDef(DataDefNamesArity, SeenConstructors,
                      TempConstructors, H),
        ensureDataDefs(DataDefNamesArity, TempConstructors,
                       NewConstructors, T).
        
% -DataDef
% -Name
% -pair(Name, Arity)
datadefExtractor(datadef(Name, TypeParams, _), Name, pair(Name, Arity)) :-
        atom(Name),
        length(TypeParams, Arity).

% -DataDefs:   [DataDef]
% -NamesArity: [pair(Name, Arity)]
dataDefNamesArity(DataDefs, NamesArity) :-
        maplist(datadefExtractor, DataDefs, DataDefNames, NamesArity),
        is_set(DataDefNames).

% -DataDefs: [DataDef]
% -NamesArity: [pair(Name, Arity)]
ensureDataDefs(DataDefs, NamesArity) :-
        ensureDataDefs(NamesArity, [], _, DataDefs).

% clausedef(map, [A], [list(A), relation([A, B]), list(B)])

% For a clause definition, we need to check the following:
%
% -Type variables are all in scope
% -No other clause definition with the same name and arity exists

% -DataDefNamesArity:  [pair(Name, Arity)]
% -ClauseDef
ensureClauseDef(DataDefNamesArity, clausedef(Name, TypeVars, ParamTypes)) :-
        atom(Name),
        ensureTypeVars(TypeVars),
        ensureTypes(ParamTypes, TypeVars, DataDefNamesArity).


% -DataDefNamesArity: [pair(Name, Arity)]
% -ClauseDefs:        [ClauseDef]
ensureClauseDefs(_, []).
ensureClauseDefs(DataDefNamesArity, [H|T]) :-
        ensureClauseDef(DataDefNamesArity, H),
        ensureClauseDefs(DataDefNamesArity, T).

% -ClauseDef: ClauseDef
% -Pair:      pair(Name, Arity)
clausedefExtractor(clausedef(Name, _, Params), pair(Name, Arity)) :-
        atom(Name),
        length(Params, Arity).

% -ClauseDefs: [ClauseDef]
% -NameArity:  [pair(Name, Arity)]
clauseDefNamesArity(ClauseDefs, NameArity) :-
        maplist(clausedefExtractor, ClauseDefs, NameArity),
        is_set(NameArity).

% -ClauseDefNameArity: [pair(Name, Arity)]
% -Clause:             Clause
ensureClause(ClauseDefNameArity, :-(Head, Body)) :-
        Head =.. [Name|Params],
        atom(Name),
        length(Params, Arity),
        member(pair(Name, Arity), ClauseDefNameArity),
        ensureTerms(ClauseDefNameArity, Params),
        ensureBody(ClauseDefNameArity, Body).

% -DataDefNamesArity: [pair(Name, Arity)]
% -GlobalVarDef:      GlobalVarDef
% -SeenGlobal:        [Name]
% -NewSeenGlobal:     [Name]
ensureGlobalVarDef(DataDefNamesArity, globalvardef(Name, TypeVars, Type),
                   Seen, [Name|Seen]) :-
        atom(Name),
        \+ member(Name, Seen),
        ensureTypeVars(TypeVars),
        ensureType(Type, TypeVars, DataDefNamesArity).

% -DataDefNamesArity: [pair(Name, Arity)]
% -GlobalVarDefs:     [GlobalVarDef]
% -SeenGlobal:        [Name]
ensureGlobalVarDefs(_, [], _).
ensureGlobalVarDefs(DataDefNamesArity, [H|T], Seen) :-
        ensureGlobalVarDef(DataDefNamesArity, H, Seen, NewSeen),
        ensureGlobalVarDefs(DataDefNamesArity, T, NewSeen).

% -DataDefNamesArity: [pair(Name, Arity)]
% -GlobalVarDefs: [GlobalVarDef]
ensureGlobalVarDefs(DataDefNamesArity, VarDefs) :-
        ensureGlobalVarDefs(DataDefNamesArity, VarDefs, []).

% -DataDefs:      [DataDef]
% -ClauseDefs:    [ClauseDef]
% -GlobalVarDefs: [GlobalVarDef]
% -Clauses:       [Clause]
ensureProgram(DataDefs, ClauseDefs, GlobalVarDefs, Clauses) :-
        dataDefNamesArity(DataDefs, DataDefNamesArity),
        clauseDefNamesArity(ClauseDefs, ClauseDefNamesArity),
        ensureDataDefs(DataDefs, DataDefNamesArity),
        ensureClauseDefs(DataDefNamesArity, ClauseDefs),
        ensureGlobalVarDefs(DataDefNamesArity, GlobalVarDefs),
        maplist(ensureClause(ClauseDefNamesArity), Clauses).
