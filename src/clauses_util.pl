:- module('clauses_util', [loadFile/2, writeClauses/2]).

:- use_module('sanitizer.pl', [sanitizeFile/1]).

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
%            ModuleDef, [UseModule], [Clause])

% -[(A)]
% -[A]
% -[[A]]
%
% Everything that didn't get partitioned will be put into the last element
partitionBy([], Rest, [Rest]).
partitionBy([H|T], Items, [Group|Rest]) :-
        partition(H, Items, Group, RestItems),
        partitionBy(T, RestItems, Rest).

% -FileName: FileName
% -ExpectedModuleName: Atom
expectedModuleName(FileName, ExpectedModuleName) :-
        file_base_name(FileName, BaseName),
        file_name_extension(ExpectedModuleName, 'pl', BaseName).

% -Filename
% -LoadedFile: loadedFile
%
% Will sanitize the given file and normalize clauses.
loadFile(Filename, LoadedFile) :-
        LoadedFile = loadedFile(DataDefs, ClauseDefs, GlobalVarDefs,
                                ModuleDef, UseModules, Clauses),
        clausesInFile(Filename, RawClauses),
        
        partitionBy([isDataDef, isClauseDef, isGlobalVarDef,
                     isModuleDef(Filename), isUseModule],
                    RawClauses,
                    [DataDefs, ClauseDefs, GlobalVarDefs,
                     [ModuleDef], UseModules, NonNormalizedClauses]),
        maplist(normalizeClause, NonNormalizedClauses, Clauses),
        sanitizeFile(LoadedFile).

isDataDef(datadef(_, _, _)).
isClauseDef(clausedef(_, _, _)).
isGlobalVarDef(globalvardef(_, _, _)).
isUseModule(use_module(_, _, _)).

% -FileName
% -ModuleDef
isModuleDef(FileName, module(Name, _, _)) :-
        expectedModuleName(FileName, Name).

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
