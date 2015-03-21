module(bootstrap_main, [], []).

use_module('bootstrap_module_handler.pl', [handleModules/5], []).
use_module('bootstrap_typechecker.pl', [typecheckClauses/4], []).


clausedef(processFile, [], [atom]).
processFile(InputFile) :-
        handleModules(InputFile, DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !,
        typecheckClauses(DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !.
