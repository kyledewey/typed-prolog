module(syntax, [loadFile/2],
               [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                typeConstructor, defdata, clauseclause, defglobalvar,
                defmodule, def_use_module, loadedFile, bodyUnaryOp, unop,
                compareOp]).

use_module('io.pl', [read_clauses_from_file/3], []).
use_module('common.pl', [map/3, forall/2, setContains/2, onFailure/2,
                         yolo_UNSAFE_format_shim/2], [pair]).

% BEGIN AST DEFINITION
%
% The whole int hackery works because variables will never be instantiated.
datadef(op, [], [plus, minus, mul, div, op_min, op_max,
                 shift_left, shift_right, bitwise_and, bitwise_or,
                 int_div, int_rem,
                 int_mod, op_exponent]).
datadef(unop, [], [op_msb, op_abs, op_truncate]).
datadef(exp, [], [exp_var(int),
                  exp_num(int),
                  binop(exp, op, exp),
                  unaryop(unop, exp)]).
datadef(expLhs, [], [lhs_var(int), lhs_num(int)]).
datadef(term, [], [term_var(int), term_num(int),
                   term_lambda(list(term), body),
                   term_constructor(atom, list(term))]).

datadef(bodyUnaryOp, [], [not]).
datadef(bodyPairOp, [], [and, or, implies]).
datadef(compareOp, [], [    lt,     lte,     gt,     gte,
                        clp_lt, clp_lte, clp_gt, clp_gte,
                        clp_eq, clp_neq]).
datadef(body, [], [body_is(expLhs, exp),
                   body_setvar(atom, term), body_getvar(atom, term),
                   bodyUnary(bodyUnaryOp, body),
                   bodyPair(body, bodyPairOp, body),
                   bodyComparison(exp, compareOp, exp),
                   higherOrderCall(term, list(term)),
                   firstOrderCall(atom, list(term))]).

datadef(type, [], [intType, % integers
                   atomType, % raw atoms
                   relationType(list(type)), % a relation (higher-order clause)
                   constructorType(atom, list(type)), % constructor for a user-defined type
                   typevar(int)]). % placeholder for a parametric type.  Only exists
                                   % during typechecking.

datadef(defclause, [], [defclause(atom, % name of the clause
                                  list(type), % generic type parameters.
                                              % Should be variables.
                                  list(type))]). % parameter types
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

% unsafe because of the use of atom
clausedef(yolo_UNSAFE_allAtoms, [A], [list(A)]).
yolo_UNSAFE_allAtoms(List) :-
        forall(List, lambda([A], atom(A))).

clausedef(yolo_UNSAFE_translate_exp_lhs, [A], [A, expLhs]).
yolo_UNSAFE_translate_exp_lhs(Var, lhs_var(NewVar)) :-
        var(Var),
        !,
        Var = NewVar.
yolo_UNSAFE_translate_exp_lhs(Num, lhs_num(NewNum)) :-
        number(Num),
        !,
        Num = NewNum.

% The Op hackery is needed to bypass the typechecker.  Currently
% parameters get typechecked but bodies don't.
clausedef(yolo_UNSAFE_translate_op, [A], [A, op]).
yolo_UNSAFE_translate_op(Op, plus) :- Op = '+', !.
yolo_UNSAFE_translate_op(Op, minus) :- Op = '-', !.
yolo_UNSAFE_translate_op(Op, mul) :- Op = '*', !.
yolo_UNSAFE_translate_op(Op, div) :- Op = '/', !.
yolo_UNSAFE_translate_op(Op, op_min) :- Op = min, !.
yolo_UNSAFE_translate_op(Op, op_max) :- Op = max, !.
yolo_UNSAFE_translate_op(Op, shift_left) :- Op = '<<', !.
yolo_UNSAFE_translate_op(Op, shift_right) :- Op = '>>', !.
yolo_UNSAFE_translate_op(Op, bitwise_and) :- Op = '/\\', !.
yolo_UNSAFE_translate_op(Op, bitwise_or) :- Op = '\\/', !.
yolo_UNSAFE_translate_op(Op, int_div) :- Op = '//', !.
yolo_UNSAFE_translate_op(Op, int_rem) :- Op = rem, !.
yolo_UNSAFE_translate_op(Op, int_mod) :- Op = mod, !.
yolo_UNSAFE_translate_op(Op, op_exponent) :- Op = '^', !.

clausedef(yolo_UNSAFE_translate_unop, [A], [A, unop]).
yolo_UNSAFE_translate_unop(Op, op_msb) :- Op = msb, !.
yolo_UNSAFE_translate_unop(Op, op_abs) :- Op = abs, !.
yolo_UNSAFE_translate_unop(Op, op_truncate) :- Op = truncate, !.

clausedef(yolo_UNSAFE_translate_exp, [A], [A, exp]).
yolo_UNSAFE_translate_exp(Var, exp_var(NewVar)) :-
        var(Var),
        !,
        Var = NewVar.
yolo_UNSAFE_translate_exp(Num, exp_num(NewNum)) :-
        number(Num),
        !,
        Num = NewNum.
yolo_UNSAFE_translate_exp(Structure, binop(Exp1, Op, Exp2)) :-
        Structure =.. [RawOp, E1, E2],
        !,
        yolo_UNSAFE_translate_op(RawOp, Op),
        yolo_UNSAFE_translate_exp(E1, Exp1),
        yolo_UNSAFE_translate_exp(E2, Exp2).
yolo_UNSAFE_translate_exp(Structure, unaryop(Op, Exp)) :-
        Structure =.. [RawOp, E],
        !,
        yolo_UNSAFE_translate_unop(RawOp, Op),
        yolo_UNSAFE_translate_exp(E, Exp).

clausedef(yolo_UNSAFE_translate_body_pair_op, [A], [A, bodyPairOp]).
yolo_UNSAFE_translate_body_pair_op(Op, and) :- Op = ',', !.
yolo_UNSAFE_translate_body_pair_op(Op, or) :- Op = ';', !.
yolo_UNSAFE_translate_body_pair_op(Op, implies) :- Op = '->', !.

clausedef(yolo_UNSAFE_translate_unary_body_op, [A], [A, bodyUnaryOp]).
yolo_UNSAFE_translate_unary_body_op(Op, not) :- Op = '\\+', !.

clausedef(yolo_UNSAFE_translate_compare_op, [A], [A, compareOp]).
yolo_UNSAFE_translate_compare_op(Op, lt) :- Op = '<', !.
yolo_UNSAFE_translate_compare_op(Op, lte) :- Op = '=<', !.
yolo_UNSAFE_translate_compare_op(Op, gt) :- Op = '>', !.
yolo_UNSAFE_translate_compare_op(Op, gte) :- Op = '>=', !.
yolo_UNSAFE_translate_compare_op(Op, clp_lt) :- Op = '#<', !.
yolo_UNSAFE_translate_compare_op(Op, clp_lte) :- Op = '#=<', !.
yolo_UNSAFE_translate_compare_op(Op, clp_gt) :- Op = '#>', !.
yolo_UNSAFE_translate_compare_op(Op, clp_gte) :- Op = '#>=', !.
yolo_UNSAFE_translate_compare_op(Op, clp_eq) :- Op = '#=', !.
yolo_UNSAFE_translate_compare_op(Op, clp_neq) :- Op = '#\\=', !.

clausedef(translateBody, [A], [A, body]).
translateBody(Input, Output) :-
        onFailure(
            lambda([], yolo_UNSAFE_translate_body(Input, Output)),
            lambda([], yolo_UNSAFE_format_shim('Syntax error in body: ~w~n~n', [Input]))).

clausedef(yolo_UNSAFE_translate_body, [A], [A, body]).
yolo_UNSAFE_translate_body(Input, body_is(NewExpLhs, NewExp)) :-
        Input = is(ExpLhs, Exp),
        !,
        yolo_UNSAFE_translate_exp_lhs(ExpLhs, NewExpLhs),
        yolo_UNSAFE_translate_exp(Exp, NewExp).
yolo_UNSAFE_translate_body(Input, bodyComparison(NewExp1, CompareOp, NewExp2)) :-
        Input =.. [Op, Exp1, Exp2],
        yolo_UNSAFE_translate_compare_op(Op, CompareOp),
        !,
        yolo_UNSAFE_translate_exp(Exp1, NewExp1),
        yolo_UNSAFE_translate_exp(Exp2, NewExp2).
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
yolo_UNSAFE_translate_body(Input, bodyUnary(NewOp, NewBody)) :-
        Input =.. [Op, Body],
        yolo_UNSAFE_translate_unary_body_op(Op, NewOp),
        !,
        translateBody(Body, NewBody).
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
            lambda([], yolo_UNSAFE_format_shim('Syntax error in term: ~w~n~n', [Input]))).

clausedef(yolo_UNSAFE_translate_term, [A], [A, term]).
yolo_UNSAFE_translate_term(Var, term_var(NewVar)) :-
        var(Var),
        !,
        Var = NewVar.
yolo_UNSAFE_translate_term(Num, term_num(NewNum)) :-
        number(Num),
        !,
        Num = NewNum.
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
yolo_UNSAFE_translate_type(_, Type, intType) :- Type = int, !.
yolo_UNSAFE_translate_type(_, Type, atomType) :- Type = atom, !.
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
            lambda([], yolo_UNSAFE_format_shim('Syntax error in type: ~w~n~n', [Input]))).

clausedef(translateClause, [A], [A, readclause]).
translateClause(Clause, ReadClause) :-
        onFailure(
            lambda([], yolo_UNSAFE_translate_clause(Clause, ReadClause)),
            lambda([], yolo_UNSAFE_format_shim('Syntax error in clause: ~w~n~n', [Clause]))).

clausedef(yolo_UNSAFE_translate_clause, [A], [A, readclause]).
yolo_UNSAFE_translate_clause(
        Input,
        readDefModule(defmodule(Name, ProcessedExportedClauses, ExportedData))) :-
        Input = module(Name, RawExportedClauses, ExportedData),
        !,
        atom(Name),
        yolo_UNSAFE_translate_pairs(RawExportedClauses, ProcessedExportedClauses),
        yolo_UNSAFE_allAtoms(ExportedData).
yolo_UNSAFE_translate_clause(
        Input,
        readDefUseModule(def_use_module(Name, ProcessedImportedClauses, ImportedData))) :-
        Input = use_module(Name, RawImportedClauses, ImportedData),
        !,
        atom(Name),
        yolo_UNSAFE_translate_pairs(RawImportedClauses, ProcessedImportedClauses),
        yolo_UNSAFE_allAtoms(ImportedData).
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
