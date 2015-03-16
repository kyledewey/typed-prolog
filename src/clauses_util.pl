:- module('clauses_util', [clausesFromFilesWithBuiltins/4, writeClauses/2]).

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
% -ClausesInput:  [Clause]
% -ClausesOutput: [Clause]
clausesInStream(Stream, ClausesInput, ClausesOutput) :-
        read_clause(Stream, Clause, []),
        ((Clause == end_of_file) ->
            (ClausesInput = ClausesOutput);
            (ClausesInput = [Clause|Rest],
             clausesInStream(Stream, Rest, ClausesOutput))).

% -Filename
% -ClausesInput:  [Clause]
% -ClausesOutput: [Clause]
clausesInFile(Filename, ClausesInput, ClausesOutput) :-
        open(Filename, read, Stream),
        clausesInStream(Stream, ClausesInput, ClausesOutput),
        close(Stream).

% -[Filename]
% -ClausesInput:  [Clause]
% -ClausesOutput: [Clause]
clausesInFiles([], Clauses, Clauses).
clausesInFiles([H|T], InputClauses, OutputClauses) :-
        clausesInFile(H, InputClauses, TempClauses),
        clausesInFiles(T, TempClauses, OutputClauses).

% -[Filename]
% -Clauses: [Clause]
clausesInFiles(Files, Clauses) :-
        clausesInFiles(Files, Clauses, []).

isDataDef(datadef(_, _, _)).
isClauseDef(clausedef(_, _, _)).

% -[Filename]
% -DataDefs:   [DataDef]
% -ClauseDefs: [ClauseDef]
% -Clauses:    [Clause]
%
% All the resulting clauses have :- in the head.  Includes builtins
clausesFromFilesWithBuiltins(Files, DataDefs, ClauseDefs, NormalizedClauses) :-
        clausesInFiles(Files, Clauses1),
        
        % extract out into data definitions, clause definitions, and everything
        % else.
        partition(isDataDef, Clauses1, UserDataDefs, Clauses2),
        partition(isClauseDef, Clauses2, UserClauseDefs, Clauses3),
        builtinDataDefs(BuiltinDataDefs),
        builtinClauseDefs(BuiltinClauseDefs),
        append(BuiltinDataDefs, UserDataDefs, DataDefs),
        append(BuiltinClauseDefs, UserClauseDefs, ClauseDefs),
        maplist(normalizeClause, Clauses3, NormalizedClauses).

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
