:- use_module('clauses_util.pl', [clausesFromFilesWithBuiltins/5, writeClauses/2]).
:- use_module('sanitizer.pl', [ensureProgram/4]).
:- use_module('typechecker.pl', [typecheckClauses/4]).
:- use_module('translator.pl', [translateClauses/3]).

% -InputFiles: [Filename]
% -Engine:     swipl | gnuprolog
% -Translated: [TranslatedClause]
processFilesToClauses(Files, Engine, TranslatedClauses) :-
        clausesFromFilesWithBuiltins(Files, DataDefs, ClauseDefs,
                                     GlobalVarDefs, Clauses), !,
        ensureProgram(DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        typecheckClauses(DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        translateClauses(Clauses, Engine, TranslatedClauses), !.

% -InputFiles: [Filename]
% -Engine:     swipl | gnuprolog
% -OutputFile: Filename
processFiles(Files, Engine, OutputFile) :-
        processFilesToClauses(Files, Engine, TranslatedClauses),
        writeClauses(TranslatedClauses, OutputFile).
