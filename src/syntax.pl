module(syntax, [loadFile/2],
               [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                typeConstructor, defdata, clauseclause, defglobalvar,
                defmodule, def_use_module, loadedFile]).

use_module('io.pl', [read_clauses_from_file/3], []).
use_module('common.pl', [map/3, forall/2, setContains/2, onFailure/2,
                         yolo_UNSAFE_format_shim/2], [pair]).

% BEGIN AST DEFINITION
%
% The whole int hackery works because variables will never be instantiated.
datadef(op, [], [plus, minus, mul, div, op_min, op_max]).
datadef(exp, [], [exp_var(int), exp_num(int), binop(exp, op, exp)]).
datadef(expLhs, [], [lhs_var(int), lhs_num(int)]).
datadef(term, [], [term_var(int), term_num(int),
                   term_lambda(list(term), body),
                   term_constructor(atom, list(term))]).

datadef(bodyPairOp, [], [and, or, implies]).
datadef(body, [], [body_is(expLhs, exp),
                   body_setvar(atom, term), body_getvar(atom, term),
                   bodyPair(body, bodyPairOp, body),
                   higherOrderCall(term, list(term)),
                   firstOrderCall(atom, list(term))]).

datadef(type, [], [intType, atomType, relationType(list(type)),
                   constructorType(atom, list(type))]).

datadef(defclause, [], [defclause(atom, list(type), list(type))]).
datadef(typeConstructor, [], [typeConstructor(atom, list(type))]).

datadef(defdata, [], [defdata(atom, list(type), list(typeConstructor))]).
datadef(clauseclause, [], [clauseclause(atom, list(term), body)]).
datadef(defglobalvar, [], [defglobalvar(atom, list(type), type)]).
datadef(defmodule, [], [defmodule(atom, list(pair(atom, int)), list(atom))]).
datadef(def_use_module, [], [def_use_module(atom, list(pair(atom, int)), list(atom))]).
datadef(loadedFile, [], [loadedFile(defmodule, list(def_use_module),
                                    list(defdata), list(defclause), list(defglobalvar),
                                    list(clauseclause))]).
% END AST DEFINITION
datadef(readclause, [], [readDefModule(defmodule), readDefUseModule(def_use_module),
                         readDefData(defdata), readDefClause(defclause),
                         readDefGlobalVar(defglobalvar), readClauseClause(clauseclause)]).

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
yolo_UNSAFE_translate_op(min, op_min).
yolo_UNSAFE_translate_op(max, op_max).

clausedef(yolo_UNSAFE_translate_exp, [A], [A, exp]).
yolo_UNSAFE_translate_exp(Var, exp_var(Var)) :-
        var(Var),
        !.
yolo_UNSAFE_translate_exp(Num, exp_num(Num)) :-
        number(Num),
        !.
yolo_UNSAFE_translate_exp(Structure, binop(Exp1, Op, Exp2)) :-
        Structure =.. [RawOp, E1, E2],
        !,
        yolo_UNSAFE_translate_op(RawOp, Op),
        yolo_UNSAFE_translate_exp(E1, Exp1),
        yolo_UNSAFE_translate_exp(E2, Exp2).

clausedef(yolo_UNSAFE_translate_body_pair_op, [A], [A, bodyPairOp]).
yolo_UNSAFE_translate_body_pair_op(',', and).
yolo_UNSAFE_translate_body_pair_op(';', or).
yolo_UNSAFE_translate_body_pair_op('->', implies).

clausedef(translateBody, [A], [A, body]).
translateBody(Input, Output) :-
        onFailure(
            lambda([], yolo_UNSAFE_translate_body(Input, Output)),
            lambda([], yolo_UNSAFE_format_shim('Syntax error in body: ~w~n', [Input]))).

clausedef(yolo_UNSAFE_translate_body, [A], [A, body]).
yolo_UNSAFE_translate_body(Input, body_is(NewExpLhs, NewExp)) :-
        Input = is(ExpLhs, Exp),
        !,
        yolo_UNSAFE_translate_exp_lhs(ExpLhs, NewExpLhs),
        yolo_UNSAFE_translate_exp(Exp, NewExp).
yolo_UNSAFE_translate_body(Input, body_setvar(VarName, NewTerm)) :-
        Input = setvar(VarName, Term),
        !,
        atom(VarName),
        translateTerm(Term, NewTerm).
yolo_UNSAFE_translate_body(Input, body_getvar(VarName, NewTerm)) :-
        Input = getvar(VarName, Term),
        !,
        atom(VarName),
        translateTerm(Term, NewTerm).
yolo_UNSAFE_translate_body(Input, bodyPair(Body1, NewBodyOp, Body2)) :-
        Input =.. [BodyOp, B1, B2],
        yolo_UNSAFE_translate_body_pair_op(BodyOp, NewBodyOp),
        !,
        translateBody(B1, Body1),
        translateBody(B2, Body2).
yolo_UNSAFE_translate_body(Input, higherOrderCall(NewWhat, NewParams)) :-
        Input =.. [call, What|Params],
        !,
        translateTerm(What, NewWhat),
        translateTerms(Params, NewParams).
yolo_UNSAFE_translate_body(Input, firstOrderCall(Name, NewParams)) :-
        Input =.. [Name|Params],
        !,
        translateTerms(Params, NewParams).

clausedef(translateTerms, [A], [list(A), list(term)]).
translateTerms(Input, Output) :-
        map(Input, lambda([I, O], translateTerm(I, O)), Output).

clausedef(translateTerm, [A], [A, term]).
translateTerm(Input, Output) :-
        onFailure(
            lambda([], yolo_UNSAFE_translate_term(Input, Output)),
            lambda([], yolo_UNSAFE_format_shim('Syntax error in term: ~w~n', [Input]))).

clausedef(yolo_UNSAFE_translate_term, [A], [A, term]).
yolo_UNSAFE_translate_term(Var, term_var(Var)) :-
        var(Var),
        !.
yolo_UNSAFE_translate_term(Num, term_num(Num)) :-
        number(Num),
        !.
yolo_UNSAFE_translate_term(Input, term_lambda(NewParams, NewBody)) :-
        Input =.. [lambda, Params, Body], % differentiate from metalanguage lambdas
        !,
        translateTerms(Params, NewParams),
        translateBody(Body, NewBody).
yolo_UNSAFE_translate_term(Input, term_constructor(Name, NewParams)) :-
        Input =.. [Name|Params],
        translateTerms(Params, NewParams).

clausedef(yolo_UNSAFE_normalize_clause, [A, B], [A, B]).
yolo_UNSAFE_normalize_clause(Input, Input) :-
        Input = :-(_, _),
        !.
yolo_UNSAFE_normalize_clause(Clause, Output) :-
        Output = :-(Clause, true).

clausedef(translateTypes, [A, B], [list(A), list(B), list(type)]).
translateTypes(TypeVars, Inputs, Outputs) :-
        map(Inputs, lambda([I, O], translateType(TypeVars, I, O)), Outputs).

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
        translateTypes(TypeVars, Types, NewTypes).
yolo_UNSAFE_translate_type(TypeVars, Constructor, constructorType(Name, NewTypes)) :-
        Constructor =.. [Name|Types],
        !,
        translateTypes(TypeVars, Types, NewTypes).

clausedef(translateType, [A, B], [list(A), B, type]).
translateType(TypeVars, Input, Output) :-
        onFailure(
            lambda([], yolo_UNSAFE_translate_type(TypeVars, Input, Output)),
            lambda([], yolo_UNSAFE_format_shim('Syntax error in type: ~w~n', [Input]))).

clausedef(translateClause, [A], [A, readclause]).
translateClause(Clause, ReadClause) :-
        onFailure(
            lambda([], yolo_UNSAFE_translate_clause(Clause, ReadClause)),
            lambda([], yolo_UNSAFE_format_shim('Syntax error in clause: ~w~n', [Clause]))).

clausedef(yolo_UNSAFE_translate_clause, [A], [A, readclause]).
yolo_UNSAFE_translate_clause(
        Input,
        readDefModule(defmodule(Name, ProcessedExportedClauses, ExportedData))) :-
        Input = module(Name, RawExportedClauses, ExportedData),
        !,
        atom(Name),
        yolo_UNSAFE_translate_pairs(RawExportedClauses, ProcessedExportedClauses),
        allAtoms(ExportedData).
yolo_UNSAFE_translate_clause(
        Input,
        readDefUseModule(def_use_module(Name, ProcessedImportedClauses, ImportedData))) :-
        Input = use_module(Name, RawImportedClauses, ImportedData),
        !,
        atom(Name),
        yolo_UNSAFE_translate_pairs(RawImportedClauses, ProcessedImportedClauses),
        allAtoms(ImportedData).
yolo_UNSAFE_translate_clause(
        Input,
        readDefData(defdata(Name, TypeVars, ProcessedConstructors))) :-
        Input = datadef(Name, TypeVars, RawConstructors),
        !,
        atom(Name),
        areTypeVars(TypeVars),
        map(RawConstructors,
            lambda([Cons, typeConstructor(ConstructorName, NewTypes)],
                (Cons =.. [ConstructorName|Types],
                 translateTypes(TypeVars, Types, NewTypes))),
            ProcessedConstructors).
yolo_UNSAFE_translate_clause(
        Input,
        readDefClause(defclause(Name, TypeVars, NewTypes))) :-
        Input = clausedef(Name, TypeVars, Types),
        !,
        atom(Name),
        areTypeVars(TypeVars),
        translateTypes(TypeVars, Types, NewTypes).
yolo_UNSAFE_translate_clause(
        Input,
        readDefGlobalVar(defglobalvar(Name, TypeVars, NewType))) :-
        Input = globalvardef(Name, TypeVars, Type),
        !,
        atom(Name),
        areTypeVars(TypeVars),
        translateType(TypeVars, Type, NewType).
yolo_UNSAFE_translate_clause(
        RawClause,
        readClauseClause(clauseclause(Name, NewParams, NewBody))) :-
        yolo_UNSAFE_normalize_clause(RawClause, :-(Head, Body)),
        Head =.. [Name|Params],
        translateTerms(Params, NewParams),
        translateBody(Body, NewBody).

clausedef(sortClause, [], [readclause,
                           list(defmodule), list(defmodule),
                           list(def_use_module), list(def_use_module),
                           list(defdata), list(defdata),
                           list(defclause), list(defclause),
                           list(defglobalvar), list(defglobalvar),
                           list(clauseclause), list(clauseclause)]).
sortClause(readDefModule(DefMod),
           [DefMod|RestDefMod], RestDefMod,
           DefUse, DefUse,
           DefData, DefData,
           DefClause, DefClause,
           DefGlobalVar, DefGlobalVar,
           ClauseClause, ClauseClause).
sortClause(readDefUseModule(DefUseMod),
           DefMod, DefMod,
           [DefUseMod|RestDefUse], RestDefUse,
           DefData, DefData,
           DefClause, DefClause,
           DefGlobalVar, DefGlobalVar,
           ClauseClause, ClauseClause).
sortClause(readDefData(DefData),
           DefMod, DefMod,
           DefUse, DefUse,
           [DefData|RestDefData], RestDefData,
           DefClause, DefClause,
           DefGlobalVar, DefGlobalVar,
           ClauseClause, ClauseClause).
sortClause(readDefClause(DefClause),
           DefMod, DefMod,
           DefUse, DefUse,
           DefData, DefData,
           [DefClause|RestDefClause], RestDefClause,
           DefGlobalVar, DefGlobalVar,
           ClauseClause, ClauseClause).
sortClause(readDefGlobalVar(DefGlobalVar),
           DefMod, DefMod,
           DefUse, DefUse,
           DefData, DefData,
           DefClause, DefClause,
           [DefGlobalVar|RestDefGlobalVar], RestDefGlobalVar,
           ClauseClause, ClauseClause).
sortClause(readClauseClause(ClauseClause),
           DefMod, DefMod,
           DefUse, DefUse,
           DefData, DefData,
           DefClause, DefClause,
           DefGlobalVar, DefGlobalVar,
           [ClauseClause|RestClauseClause], RestClauseClause).

clausedef(sortClauses, [], [list(readclause),
                            list(defmodule), list(defmodule),
                            list(def_use_module), list(def_use_module),
                            list(defdata), list(defdata),
                            list(defclause), list(defclause),
                            list(defglobalvar), list(defglobalvar),
                            list(clauseclause), list(clauseclause)]).
sortClauses([],
            DefMod, DefMod,
            DefUse, DefUse,
            DefData, DefData,
            DefClause, DefClause,
            DefGlobalVar, DefGlobalVar,
            ClauseClause, ClauseClause).
sortClauses([H|T],
            DefMod, NewDefMod,
            DefUse, NewDefUse,
            DefData, NewDefData,
            DefClause, NewDefClause,
            DefGlobalVar, NewDefGlobalVar,
            ClauseClause, NewClauseClause) :-
        sortClause(H,
                   DefMod, TempDefMod,
                   DefUse, TempDefUse,
                   DefData, TempDefData,
                   DefClause, TempDefClause,
                   DefGlobalVar, TempDefGlobalVar,
                   ClauseClause, TempClauseClause),
        sortClauses(T, 
                    TempDefMod, NewDefMod,
                    TempDefUse, NewDefUse,
                    TempDefData, NewDefData,
                    TempDefClause, NewDefClause,
                    TempDefGlobalVar, NewDefGlobalVar,
                    TempClauseClause, NewClauseClause).

clausedef(loadFile, [], [atom, loadedFile]).
loadFile(Filename, loadedFile(DefModule, DefUseModule,
                              DefData, DefClause, DefGlobalVar,
                              ClauseClause)) :-
        read_clauses_from_file(
            Filename,
            lambda([A, B], translateClause(A, B)),
            ReadClauses),
        sortClauses(ReadClauses,
                    [DefModule], [],
                    DefUseModule, [],
                    DefData, [],
                    DefClause, [],
                    DefGlobalVar, [],
                    ClauseClause, []).
