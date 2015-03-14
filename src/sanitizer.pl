% The syntax we consider is more restrictive than everything in Prolog.
% Most importantly, we have a distinction between code and data, with
% lambdas being a well-defined intermediary between the two.  These
% added restrictions prevent us from using code directly as data, so
% we can actually typecheck things.

ensureTerms(Terms) :-
        maplist(ensureTerm, Terms).

ensureTerm(Var) :-
        var(Var),
        !.
ensureTerm(Atom) :-
        atom(Atom),
        !.
ensureTerm(Int) :-
        number(Int),
        !.
ensureTerm(Lambda) :-
        Lambda =.. [lambda, Params, Body],
        !,
        ensureTerms(Params),
        ensureBody(Body).
ensureTerm(Structure) :-
        !,
        ensureStructure(Structure).

ensureStructure(Structure) :-
        Contents = [_|_],
        Structure =.. [_|Contents],
        ensureTerms(Contents).

ensureBody((B1, B2)) :-
        !,
        ensureBody(B1),
        ensureBody(B2).
ensureBody((B1; B2)) :-
        !,
        ensureBody(B1),
        ensureBody(B2).
ensureBody(=(T1, T2)) :-
        !,
        ensureTerms([T1, T2]).
ensureBody(->(B1, B2)) :-
        !,
        ensureBody(B1),
        ensureBody(B2).
ensureBody(Call) :-
        !,
        (atom(Call) ->
            true;
            ensureStructure(Call)).

% like member, except it uses == instead of =
memberEqual(A, [H|T]) :-
        A == H; memberEqual(A, T).


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
ensureType(int, _, _) :- !.
ensureType(relation(Types), TypeVarsInScope, DataDefNamesArity) :-
        !,
        ensureTypes(Types, TypeVarsInScope, DataDefNamesArity).
ensureType(TypeVar, TypeVarsInScope, _) :-
        var(TypeVar),
        !,
        memberEqual(TypeVar, TypeVarsInScope).
ensureType(ConstructorType, TypeVarsInScope, DataDefNamesArity) :-
        ConstructorType =.. [Name, Types],
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

% clausedef(map, [A], [list(A), relation(A, B), list(B)])

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
        ensureTerms(Params),
        ensureBody(Body).

% -DataDefs:   [DataDef]
% -ClauseDefs: [ClauseDef]
% -Clauses:    [Clause]
ensureProgram(DataDefs, ClauseDefs, Clauses) :-
        dataDefNamesArity(DataDefs, DataDefNamesArity),
        clauseDefNamesArity(ClauseDefs, ClauseDefNamesArity),
        ensureDataDefs(DataDefs, DataDefNamesArity),
        ensureClauseDefs(DataDefs, ClauseDefs),
        maplist(ensureClause(ClauseDefNamesArity), Clauses).
