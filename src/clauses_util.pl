:- module('clauses_util', [loadFileWithBuiltins/2, writeClauses/2]).

builtinDataDef(datadef(list, [A], [.(A, list(A)), []])).

builtinDataDefs(DataDefs) :-
        findall(D, builtinDataDef(D), DataDefs).

builtinClauseDef(clausedef(>, [], [int, int])).
builtinClauseDef(clausedef(<, [], [int, int])).
builtinClauseDef(clausedef(=<, [], [int, int])).
builtinClauseDef(clausedef(>=, [], [int, int])).
builtinClauseDef(clausedef(=, [A], [A, A])).
builtinClauseDef(clausedef(==, [A], [A, A])).

builtinClauseDefs(ClauseDefs) :-
        findall(C, builtinClauseDef(C), ClauseDefs).

% -InputClause:  Clause
% -OutputClause: Head :- Body.
%
% Read in clauses will either be a head or a full horn clause.
% This will make everything a horn clause.
normalizeClause(:-(Head, Body), :-(Head, Body)) :- !.
normalizeClause(Clause, :-(Clause, true)).

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
        open(Filename, read, Stream),
        clausesInStream(Stream, Clauses),
        close(Stream).

% ExportedClause: Name/Arity
% ExportedData:   TypeName
% ModuleDef: module(Name, [ExportedClause], [ExportedData])
% UseModule: use_module(FileName, [ExportedClause], [ExportedData])
% loadedFile([DataDef], [ClauseDef], [GlobalVarDef],
%            option(ModuleDef), [UseModule], [Clause])

% -[(A)]
% -[A]
% -[[A]]
%
% Everything that didn't get partitioned will be put into the last element
partitionBy([], Rest, [Rest]).
partitionBy([H|T], Items, [Group|Rest]) :-
        partition(H, Items, Group, RestItems),
        partitionBy(T, RestItems, Rest).

% -[ModuleDef]
% -option(ModuleDef)
moduleDefsListToOptionModuleDef([], none).
moduleDefsListToOptionModuleDef([ModuleDef], some(ModuleDef)).

% -Filename
% -LoadedFile: loadedFile
%
% Does not sanitize the file.  Will normalize clauses
loadFile(
        Filename,
        loadedFile(DataDefs, ClauseDefs, GlobalVarDefs,
                   ModuleDef, UseModules, Clauses)) :-
        clausesInFile(Filename, Clauses1),
        partitionBy([isDataDef, isClauseDef, isGlobalVarDef, isModuleDef, isUseModule],
                    Clauses1,
                    [DataDefs, ClauseDefs, GlobalVarDefs, ModuleDefsList, UseModules,
                     RawClauses]),
        moduleDefsListToOptionModuleDef(ModuleDefsList, ModuleDef),
        maplist(normalizeClause, RawClauses, Clauses).

isDataDef(datadef(_, _, _)).
isClauseDef(clausedef(_, _, _)).
isGlobalVarDef(globalvardef(_, _, _)).
isModuleDef(module(_, _, _)).
isUseModule(use_module(_, _, _)).

% -Filename
% -LoadedFile
%
% All the resulting clauses have :- in the head.  Includes builtins
loadFileWithBuiltins(
        Filename,
        loadedFile(DataDefs, ClauseDefs, GlobalVarDefs, ModuleDef,
                   UseModules, Clauses)) :-
        loadFile(Filename, loadedFile(UserDataDefs, UserClauseDefs, GlobalVarDefs,
                                      ModuleDef, UseModules, Clauses)),
        builtinDataDefs(BuiltinDataDefs),
        builtinClauseDefs(BuiltinClauseDefs),
        append(BuiltinDataDefs, UserDataDefs, DataDefs),
        append(BuiltinClauseDefs, UserClauseDefs, ClauseDefs).

% -What: Clause
% -To:   Stream
writeClause(Clause, Stream) :-
        copy_term(Clause, Copy),
        numbervars(Copy, 0, _, [singletons(true)]),
        write_term(Stream, Copy, [numbervars(true)]),
        format(Stream, '.~n', []).

% -To:   Stream
% -What: Clause
writeClause_(Stream, Clause) :-
        writeClause(Clause, Stream).

% -Clauses: [Clause]
% -Filename
writeClauses(Clauses, Filename) :-
        open(Filename, write, Stream),
        maplist(writeClause_(Stream), Clauses),
        close(Stream).
