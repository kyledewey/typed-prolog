module(bootstrap_typechecker, [], []).

use_module('bootstrap_syntax.pl', [], [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                                       typeConstructor, defdata, clauseclause, defglobalvar,
                                       defmodule, def_use_module, loadedFile]).
use_module('common.pl', [map/3, flatMap/3], [pair]).

clausedef(builtinDataDefs, [], [list(defdata)]).
builtinDataDefs(
        [defdata(list, [A], [typeConstructor('.', [A, constructorType(list, [A])]),
                             typeConstructor([], [])])]).

% Even though these all are technically working with the same type variable,
% given that we'll copy terms this doesn't hurt anything
clausedef(builtinClauseDefs, [], [list(defclause)]).
builtinClauseDefs(
        [defclause(var, [A], [A]),
         defclause(atom, [A], [A]),
         defclause(>, [], [intType, intType]),
         defclause(<, [], [intType, intType]),
         defclause(=<, [], [intType, intType]),
         defclause(>=, [], [intType, intType]),
         defclause(=, [A], [A, A]),
         defclause(==, [A], [A, A]),
         defclause(is_set, [A], [constructorType(list, [A])]),
         defclause(member, [A], [A, constructorType(list, [A])]),
         defclause(copy_term, [A], [A, A]),
         defclause(append, [A], [constructorType(list, [A]),
                                 constructorType(list, [A]),
                                 constructorType(list, [A])])
        ]).

clausedef(keys, [A, B], [list(pair(A, B)), list(A)]).
keys(Pairs, Keys) :-
        map(Pairs, lambda([pair(Key, _), Key], true), Keys).

% succeeds if the provided mapping is unique
clausedef(mappingUnique, [A, B], [list(pair(A, B))]).
mappingUnique(Pairs) :-
        keys(Pairs, Keys),
        is_set(Keys).

% gets a mapping of constructor names to their corresponding data defs
clausedef(constructorToDataDefMapping, [], [list(defdata), list(pair(atom, defdata))]).
constructorToDataDefMapping(DefDatas, Mapping) :-
        flatMap(
            DefDatas,
            lambda([DefData, DefDataResult],
                (DefData = defdata(_, _, TypeConstructors),
                 map(TypeConstructors,
                     lambda([typeConstructor(Name, _), pair(Name, DefData)], true),
                     DefDataResult))),
            Mapping),
        mappingUnique(Mapping).
