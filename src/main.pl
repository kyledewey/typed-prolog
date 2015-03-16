:- use_module('clauses_util.pl', [clausesFromFilesWithBuiltins/4, writeClauses/2]).
:- use_module('sanitizer.pl', [ensureProgram/3]).
:- use_module('typechecker.pl', [typecheckClauses/3]).
:- use_module('translator.pl', [translateClauses/2]).

% -InputFiles: [Filename]
% -Translated: [TranslatedClause]
processFilesToClauses(Files, TranslatedClauses) :-
        clausesFromFilesWithBuiltins(Files, DataDefs, ClauseDefs, Clauses), !,
        ensureProgram(DataDefs, ClauseDefs, Clauses), !,
        typecheckClauses(DataDefs, ClauseDefs, Clauses), !,
        translateClauses(Clauses, TranslatedClauses), !.

% -InputFiles: [Filename]
% -OutputFile: Filename
processFiles(Files, OutputFile) :-
        processFilesToClauses(Files, TranslatedClauses),
        writeClauses(TranslatedClauses, OutputFile).
