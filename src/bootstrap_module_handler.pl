module(bootstrap_module_handler, [], []).

use_module('common.pl', [notMember/2, foldLeft/4], []).
use_module('bootstrap_syntax.pl', [loadFile/2],
                                  [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                                   typeConstructor, defdata, clauseclause, defglobalvar,
                                   defmodule, def_use_module, loadedFile]).

globalvardef(counter, [], int).
clausedef(freshModuleId, [], [int]).
freshModuleId(N) :-
        getvar(counter, N),
        NewN is N + 1,
        setvar(counter, NewN).

datadef(loadedModule, [], [loadedModule(atom, % absolute filename
                                        int, % module ID
                                        loadedFile)]).

clausedef(yolo_UNSAFE_absolute_file_name, [], [atom, % name of file
                                               atom, % relative to another file
                                               atom]). % returned filename
yolo_UNSAFE_absolute_file_name(RelativeName, RelativeTo, AbsoluteName) :-
        absolute_file_name(RelativeName, AbsoluteName, [relative_to(RelativeTo)]),
        access_file(AbsoluteName, read).

clausedef(directLoadModule, [], [atom, % absolute filename
                                 list(loadedModule), % already loaded modules
                                 list(atom), % in progress loading
                                 list(loadedModule)]). % newly loaded modules
directLoadModule(FileName, AlreadyLoaded, InProgress,
                 [loadedModule(FileName, ModuleId, LoadedFile)|RestLoaded]) :-
        loadFile(FileName, LoadedFile),
        LoadedFile = loadedFile(_, UsesModules, _, _, _, _),
        foldLeft(UsesModules, AlreadyLoaded,
                 lambda([CurLoaded, def_use_module(CurFileName, _, _), TempLoaded],
                        loadModule(CurFileName, FileName, CurLoaded, InProgress, TempLoaded)),
                 RestLoaded),
        freshModuleId(ModuleId).

clausedef(loadModule, [], [atom, % possibly relative filename
                           atom, % relative to another file
                           list(loadedModule), % already loaded modules
                           list(atom), % modules whose loading is in progress
                           list(loadedModule)]). % newly loaded modules
loadModule(RelativeFileName, RelativeTo, AlreadyLoaded, InProgress, NewLoaded) :-
        yolo_UNSAFE_absolute_file_name(RelativeFileName, RelativeTo, AbsoluteFilename),
        
        % don't allow cyclic loading, which would put us in an infinite loop
        notMember(AbsoluteFilename, InProgress),

        % if we've already loaded this module, we're done
        (member(loadedModule(AbsoluteFilename, _, _), AlreadyLoaded) ->
            (NewLoaded = AlreadyLoaded);
            (directLoadModule(AbsoluteFileName, AlreadyLoaded,
                              [AbsoluteFileName|InProgress],
                              NewLoaded))).

%% clausedef(handleModules, [], [atom, % Entry point filename
%%                               list(defdata), list(defclause),
%%                               list(defglobalvar), list(clauseclause)]).
%% handleModules(Filename, DataDefs, ClauseDefs, GlobalVarDefs, Clauses) :-
