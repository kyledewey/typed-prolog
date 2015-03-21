module(bootstrap_main, [], []).

use_module('bootstrap_syntax.pl', [loadFile/2], [loadedFile]).
use_module('bootstrap_typechecker.pl', [typecheckClauses/4], []).

clausedef(processFile, [], [atom]).
processFile(InputFile) :-
        loadFile(InputFile, loadedFile(_, _, DataDefs, ClauseDefs, GlobalVarDefs, Clauses)), !,
        typecheckClauses(DataDefs, ClauseDefs, GlobalVarDefs, Clauses), !.
