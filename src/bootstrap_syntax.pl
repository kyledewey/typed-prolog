module(bootstrap_syntax, [], [op, exp, expLhs, term, bodyPairOp, body,
                              type, clausedef, typeConstructor, datadef,
                              clause]).

use_module('io.pl', [yolo_UNSAFE_open_file/3, yolo_UNSAFE_close_file], [stream, mode]).
use_module('common.pl', [map/3, forall/2, setContains/2], [pair]).

% BEGIN AST DEFINITION
%
% The whole int hackery works because variables will never be instantiated.
datadef(op, [], [plus, minus, mul, div, min, max]).
datadef(exp, [], [exp_num(int), binop(exp, op, exp)]).
datadef(expLhs, [], [lhs_var(int), lhs_num(int)]).
datadef(term, [], [term_var(int), term_num(int),
                   term_lambda(list(term), body),
                   term_constructor(atom, list(term))]).

datadef(bodyPairOp, [], [',', ';', '->']).
datadef(body, [], [is(expLhs, exp),
                   setvar(atom, term), getvar(atom, term),
                   bodyPair(body, bodyPairOp, body),
                   higherOrderCall(term, list(term)),
                   firstOrderCall(term, list(term))]).

datadef(type, [], [intType, atomType, relationType(list(type)),
                   constructorType(atom, list(type))]).

datadef(clausedef, [], [clausedef(atom, list(int), list(type))]).
datadef(typeConstructor, [], [typeConstructor(atom, list(type))]).

datadef(datadef, [], [datadef(atom, list(int), list(typeConstructor))]).
datadef(clause, [], [clause(atom, list(term), body)]).
datadef(globalvardef, [], [globalvardef(atom, list(int), type)]).
datadef(module, [], [module(atom, list(pair(atom, int)), list(atom))]).
datadef(use_module, [], [use_module(atom, list(pair(atom, int)), list(atom))]).
datadef(loadedFile, [], [loadedFile(module, list(use_module),
                                    list(datadef), list(clausedef), list(globalvardef),
                                    list(clause))]).
% END AST DEFINITION
datadef(readclause, [], [readModule(module), readUseModule(use_module),
                         readDataDef(datadef), readClauseDef(clausedef),
                         readGlobalVarDef(globalvardef), readClause(clause)]).

clausedef(yolo_UNSAFE_translate_pairs, [A], [list(A), list(pair(atom, int))]).
yolo_UNSAFE_translate_pairs(RawPairs, TranslatedPairs) :-
        map(RawPairs,
            lambda([/(Name, Arity), pair(Name, Arity)],
                (atom(Name),
                 number(Arity))),
            TranslatedPairs).

clausedef(areTypeVars, [A], [list(A)]).
areTypeVars(List) :-
        forall(List, lambda([X], var(X))),
        is_set(List).

clausedef(allAtoms, [A], [list(A)]).
allAtoms(List) :-
        forall(List, lambda([A], atom(A))).

clausedef(yolo_UNSAFE_translate_exp_lhs, [A], [A, expLhs]).
yolo_UNSAFE_translate_exp_lhs(Var, lhs_var(Var)) :-
        var(Var),
        !.
yolo_UNSAFE_translate_exp_lhs(Num, lhs_num(Num)) :-
        number(Num),
        !.

clausedef(yolo_UNSAFE_translate_op, [A], [A, op]).
yolo_UNSAFE_translate_op('+', plus).
yolo_UNSAFE_translate_op('-', minus).
yolo_UNSAFE_translate_op('*', mul).
yolo_UNSAFE_translate_op('/', div).
yolo_UNSAFE_translate_op(min, min).
yolo_UNSAFE_translate_op(max, max).

clausedef(yolo_UNSAFE_translate_exp, [A], [A, exp]).
yolo_UNSAFE_translate_exp(Num, exp_num(Num)) :-
        number(Num),
        !.
yolo_UNSAFE_translate_exp(Structure, binop(Exp1, Op, Exp2)) :-
        Structure =.. [RawOp, E1, E2],
        !,
        yolo_UNSAFE_translate_op(RawOp, Op),
        yolo_UNSAFE_translate_exp(E1, Exp1),
        yolo_UNSAFE_translate_exp(E2, Exp2).


clausedef(yolo_UNSAFE_translate_body, [A], [A, body]).
yolo_UNSAFE_translate_body(is(ExpLhs, Exp), is(NewExpLhs, NewExp)) :-
        !,
        yolo_UNSAFE_translate_exp_lhs(ExpLhs, NewExpLhs),
        yolo_UNSAFE_translate_exp(Exp, NewExp).

clausedef(yolo_UNSAFE_translate_terms, [A], [list(A), list(term)]).
yolo_UNSAFE_translate_terms(Input, Output) :-
        map(Input, lambda([I, O], yolo_UNSAFE_translate_term(I, O)), Output).

clausedef(yolo_UNSAFE_translate_term, [A], [A, term]).
yolo_UNSAFE_translate_term(Var, term_var(Var)) :-
        var(Var),
        !.
yolo_UNSAFE_translate_term(Num, term_num(Num)) :-
        number(Num),
        !.
yolo_UNSAFE_translate_term(Input, term_lambda(NewParams, NewBody)) :-
        Input = lambda(Params, Body),
        !,
        yolo_UNSAFE_translate_terms(Params, NewParams),
        yolo_UNSAFE_translate_body(Body, NewBody).

clausedef(yolo_UNSAFE_normalize_clause, [A, B], [A, B]).
yolo_UNSAFE_normalize_clause(Input, Input) :-
        Input = :-(_, _),
        !.
yolo_UNSAFE_normalize_clause(Clause, Output) :-
        Output = :-(Clause, true).

clausedef(yolo_UNSAFE_translate_types, [A, B], [list(A), list(B), list(type)]).
yolo_UNSAFE_translate_types(TypeVars, Inputs, Outputs) :-
        map(Inputs, lambda([I, O], yolo_UNSAFE_translate_type(TypeVars, I, O)), Outputs).

clausedef(yolo_UNSAFE_translate_type, [A, B], [list(A), B, type]).
yolo_UNSAFE_translate_type(TypeVars, TypeVar, Result) :-
        var(TypeVar),
        !,
        setContains(TypeVars, TypeVar),
        TypeVar = Result.
yolo_UNSAFE_translate_type(_, int, intType) :- !.
yolo_UNSAFE_translate_type(_, atom, atomType) :- !.
yolo_UNSAFE_translate_type(TypeVars, Input, relationType(NewTypes)) :-
        Input = relation(Types),
        !,
        yolo_UNSAFE_translate_types(TypeVars, Types, NewTypes).
yolo_UNSAFE_translate_type(TypeVars, Constructor, constructorType(Name, NewTypes)) :-
        Constructor =.. [Name|Types],
        !,
        yolo_UNSAFE_translate_types(TypeVars, Types, NewTypes).

clausedef(yolo_UNSAFE_translate_clause, [A], [A, readclause]).
yolo_UNSAFE_translate_clause(
        module(Name, RawExportedClauses, ExportedData),
        readModule(module(Name, ProcessedExportedClauses, ExportedData))) :-
        !,
        atom(Name),
        yolo_UNSAFE_translate_pairs(RawExportedClauses, ProcessedExportedClauses),
        allAtoms(ExportedData).
yolo_UNSAFE_translate_clause(
        use_module(Name, RawImportedClauses, ImportedData),
        readUseModule(use_module(Name, ProcessedImportedClauses, ImportedData))) :-
        !,
        atom(Name),
        yolo_UNSAFE_translate_pairs(RawImportedClauses, ProcessedImportedClauses),
        allAtoms(ImportedData).
yolo_UNSAFE_translate_clause(
        datadef(Name, TypeVars, RawConstructors),
        readDataDef(datadef(Name, TypeVars, ProcessedConstructors))) :-
        !,
        atom(Name),
        areTypeVars(TypeVars),
        map(RawConstructors,
            lambda([Input, typeConstructor(ConstructorName, NewTypes)],
                (Input =.. [ConstructorName|Types],
                 yolo_UNSAFE_translate_types(TypeVars, Types, NewTypes))),
            ProcessedConstructors).
yolo_UNSAFE_translate_clause(
        clausedef(Name, TypeVars, Types),
        readClauseDef(clausedef(Name, TypeVars, NewTypes))) :-
        !,
        atom(Name),
        areTypeVars(TypeVars),
        yolo_UNSAFE_translate_types(TypeVars, Types, NewTypes).
yolo_UNSAFE_translate_clause(
        globalvardef(Name, TypeVars, Type),
        readGlobalVarDef(globalvardef(Name, TypeVars, NewType))) :-
        !,
        atom(Name),
        areTypeVars(TypeVars),
        yolo_UNSAFE_translate_type(TypeVars, Type, NewType).
%% yolo_UNSAFE_translate_clause(
%%         RawClause,
%%         readClase(clause(Name, NewParams, NewBody))) :-
%%         yolo_UNSAFE_normalize_clause(RawClause, :-(Head, Body)),
%%         Head =.. [Name|Params],
        

%% clausedef(yolo_UNSAFE_load_file, [atom, loadedFile]).
%% yolo_UNSAFE_load_file(Filename, loadedFile(Module, UseModule, DataDefs, ClauseDefs,
%%                                            GlobalBarDefs, Clauses)) :-
%%         TranslateType = lambda([
        