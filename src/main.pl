module(main, [processFileForSwipl/2, processFileForGnuProlog/2], []).

use_module('module_handler.pl', [handleModules/5], []).
use_module('typechecker.pl', [typecheckClauses/4], []).
use_module('translator.pl', [translateClauses/3], [engine_type]).
use_module('printer.pl', [writeTranslatedClauses/2], []).

clausedef(processFile, [], [atom, engine_type, atom]).
processFile(InputFile, Engine, OutputFile) :-
        handleModules(InputFile, DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        typecheckClauses(DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        translateClauses(Clauses, Engine, TranslatedClauses), !,
        writeTranslatedClauses(TranslatedClauses, OutputFile), !.

% these stubs are needed so we can call them directly without having
% to manually lookup to see what they got translated to due to modules.
clausedef(processFileForSwipl, [], [atom, atom]).
processFileForSwipl(InputFile, OutputFile) :-
        processFile(InputFile, swipl, OutputFile).

clausedef(processFileForGnuProlog, [], [atom, atom]).
processFileForGnuProlog(InputFile, OutputFile) :-
        processFile(InputFile, gnuprolog, OutputFile).
