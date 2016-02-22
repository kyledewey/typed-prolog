module(printer, [writeTranslatedClauses/2], []).

use_module('common.pl', [map/3], []).
use_module('syntax.pl', [],
                        [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                         typeConstructor, defdata, clauseclause, defglobalvar,
                         defmodule, def_use_module, loadedFile, bodyUnaryOp, unop,
                         compareOp]).
use_module('io.pl', [writeClauses/2], []).

% Handles writing to files.  Assumes that translation has already occurred, so that
% it won't encounter getvar, setvar, lambda, or higher order calls.

% A whole lot of this file is unsafe because we are going down to raw Prolog
% terms, for which we no not have an associated type.

clausedef(translateOp, [], [op, atom]).
translateOp(plus, '+').
translateOp(minus, '-').
translateOp(mul, '*').
translateOp(div, '/').
translateOp(op_min, min).
translateOp(op_max, max).
translateOp(shift_left, '<<').
translateOp(shift_right, '>>').
translateOp(bitwise_and, '/\\').
translateOp(bitwise_or, '\\/').
translateOp(int_div, '//').
translateOp(int_rem, 'rem').
translateOp(int_mod, 'mod').
translateOp(op_exponent, '^').

clausedef(translateCompareOp, [], [compareOp, atom]).
translateCompareOp(lt, '<').
translateCompareOp(lte, '=<').
translateCompareOp(gt, '>').
translateCompareOp(gte, '>=').
translateCompareOp(clp_lt, '#<').
translateCompareOp(clp_lte, '#=<').
translateCompareOp(clp_gt, '#>').
translateCompareOp(clp_gte, '#>=').
translateCompareOp(clp_eq, '#=').
translateCompareOp(clp_neq, '#\\=').

clausedef(translateUnop, [], [unop, atom]).
translateUnop(op_msb, msb).
translateUnop(op_abs, abs).

clausedef(translateBodyUnaryOp, [], [bodyUnaryOp, atom]).
translateBodyUnaryOp(not, '\\+').

clausedef(translateBodyPairOp, [], [bodyPairOp, atom]).
translateBodyPairOp(and, ',').
translateBodyPairOp(or, ';').
translateBodyPairOp(implies, '->').

clausedef(yolo_UNSAFE_translate_exp, [A], [exp, A]).
yolo_UNSAFE_translate_exp(exp_var(X), NewX) :- X = NewX.
yolo_UNSAFE_translate_exp(exp_num(N), NewN) :- N = NewN.
yolo_UNSAFE_translate_exp(binop(E1, Op, E2), Output) :-
        translateOp(Op, NewOp),
        yolo_UNSAFE_translate_exp(E1, NewE1),
        yolo_UNSAFE_translate_exp(E2, NewE2),
        Output =.. [NewOp, NewE1, NewE2].
yolo_UNSAFE_translate_exp(unaryop(Op, E), Output) :-
        translateUnop(Op, NewOp),
        yolo_UNSAFE_translate_exp(E, NewE),
        Output =.. [NewOp, NewE].

clausedef(yolo_UNSAFE_translate_exp_lhs, [A], [expLhs, A]).
yolo_UNSAFE_translate_exp_lhs(lhs_var(X), NewX) :- X = NewX.
yolo_UNSAFE_translate_exp_lhs(lhs_num(N), NewN) :- N = NewN.

clausedef(translateTerms, [A], [list(term), list(A)]).
translateTerms(Terms, Result) :-
        map(Terms,
            lambda([T, R], yolo_UNSAFE_translate_term(T, R)),
            Result).

clausedef(yolo_UNSAFE_translate_term, [A], [term, A]).
yolo_UNSAFE_translate_term(term_var(X), NewX) :- X = NewX.
yolo_UNSAFE_translate_term(term_num(N), NewN) :- N = NewN.
% lambdas have been translated away
yolo_UNSAFE_translate_term(term_constructor(Name, Terms), Result) :-
        translateTerms(Terms, NewTerms),
        Result =.. [Name|NewTerms].

clausedef(yolo_UNSAFE_translate_body, [A], [body, A]).
yolo_UNSAFE_translate_body(body_is(Lhs, Exp), Result) :-
        yolo_UNSAFE_translate_exp_lhs(Lhs, NewLhs),
        yolo_UNSAFE_translate_exp(Exp, NewExp),
        Result =.. [is, NewLhs, NewExp].
yolo_UNSAFE_translate_body(bodyComparison(Exp1, Op, Exp2), Result) :-
    yolo_UNSAFE_translate_exp(Exp1, NewExp1),
    yolo_UNSAFE_translate_exp(Exp2, NewExp2),
    translateCompareOp(Op, AtomOp),
    Result =.. [AtomOp, NewExp1, NewExp2].
% setvar and getvar has been translated away
yolo_UNSAFE_translate_body(bodyUnary(Op, Body), Result) :-
        translateBodyUnaryOp(Op, NewOp),
        yolo_UNSAFE_translate_body(Body, NewBody),
        Result =.. [NewOp, NewBody].
yolo_UNSAFE_translate_body(bodyPair(B1, Op, B2), Result) :-
        translateBodyPairOp(Op, NewOp),
        yolo_UNSAFE_translate_body(B1, NewB1),
        yolo_UNSAFE_translate_body(B2, NewB2),
        Result =.. [NewOp, NewB1, NewB2].
% higher order calls have been translated away
yolo_UNSAFE_translate_body(firstOrderCall(Name, Terms), Result) :-
        translateTerms(Terms, NewTerms),
        Result =.. [Name|NewTerms].

clausedef(yolo_UNSAFE_translate_clause, [A], [clauseclause, A]).
yolo_UNSAFE_translate_clause(clauseclause(Name, Params, Body), NewClause) :-
        translateTerms(Params, NewParams),
        yolo_UNSAFE_translate_body(Body, NewBody),
        Head =.. [Name|NewParams],
        (NewBody == true ->
            (NewClause = Head);
            (NewClause =.. [':-', Head, NewBody])).

clausedef(writeTranslatedClauses, [], [list(clauseclause), atom]).
writeTranslatedClauses(Clauses, Filename) :-
        map(Clauses,
            lambda([Clause, NewClause],
                   yolo_UNSAFE_translate_clause(Clause, NewClause)),
            TranslatedClauses),
        writeClauses(TranslatedClauses, Filename).
