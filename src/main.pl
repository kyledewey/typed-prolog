:- use_module('clauses_util.pl', [writeClauses/2]).
:- use_module('module_handler.pl', [handleModules/5]).
:- use_module('typechecker.pl', [typecheckClauses/4]).
:- use_module('translator.pl', [translateClauses/3]).

% -InputFile:  Filename
% -Engine:     swipl | gnuprolog
% -OutputFile: Filename
processFile(InputFile, Engine, OutputFile) :-
        handleModules(InputFile, DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        format('DataDefs: ~w~n', [DataDefs]),
        format('ClauseDefs: ~w~n', [ClauseDefs]),
        format('GlobalVarDefs: ~w~n', [GlobalVarDefs]),
        format('Clauses: ~w~n', [Clauses]),
        typecheckClauses(DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        translateClauses(Clauses, Engine, TranslatedClauses), !,
        writeClauses(TranslatedClauses, OutputFile).
