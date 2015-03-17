:- use_module('clauses_util.pl', [loadFileWithBuiltins/2, writeClauses/2]).
:- use_module('sanitizer.pl', [sanitizeFile/1]).
:- use_module('typechecker.pl', [typecheckClauses/4]).
:- use_module('translator.pl', [translateClauses/3]).

% -InputFile:  Filename
% -Engine:     swipl | gnuprolog
% -OutputFile: Filename
processFile(InputFile, Engine, OutputFile) :-
        loadFileWithBuiltins(InputFile, LoadedFile), !,
        sanitizeFile(LoadedFile), !,
        LoadedFile = loadedFile(DataDefs, ClauseDefs, GlobalVarDefs, _, _, Clauses),
        typecheckClauses(DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        translateClauses(Clauses, Engine, TranslatedClauses), !,
        writeClauses(TranslatedClauses, OutputFile).
