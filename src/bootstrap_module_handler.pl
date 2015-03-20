module(bootstrap_module_handler, [], []).

use_module('common.pl', [notMember/2, foldLeft/4, flatMap/3, map/3, forall/2], [pair]).
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

clausedef(constructorsInDataDefs, [], [list(defdata), list(atom)]).
constructorsInDataDefs(DataDefs, Constructors) :-
        flatMap(DataDefs,
                lambda([defdata(_, _, TypeConstructors), CurConstructors],
                       map(TypeConstructors,
                           lambda([typeConstructor(Name, _), Name], true),
                           CurConstructors)),
                Constructors).

clausedef(allImportedConstructors, [], [list(loadedModule),
                                        atom, % absolute filename of original module
                                        list(def_use_module),
                                        list(atom)]).
allImportedConstructors(LoadedModules, RelativeTo, UsesModules, Constructors) :-
        flatMap(UsesModules,
                lambda([def_use_module(RelFilename, _, ImportedTypes), CurConstructors],
                       (yolo_UNSAFE_absolute_file_name(RelFilename, RelativeTo, AbsFile),
                        importedConstructors(LoadedModules, AbsFile, ImportedTypes,
                                             CurConstructors))),
                Constructors).

% Assumes that all dependencies have been loaded in.
clausedef(importedConstructors, [], [list(loadedModule), % everything loaded in so far
                                     atom, % absolute filename of imported module
                                     list(atom), % imported types
                                     list(atom)]). % imported constructors
importedConstructors(LoadedModules, Filename, ImportedTypes, Constructors) :-
        % get the corresponding module
        member(loadedModule(Filename, _, ModuleFile), LoadedModules),
        ModuleFile = loadedFile(defmodule(_, _, ExportedTypes), _, DataDefs, _, _, _),

        % ensure the module exports the types we want
        forall(ImportedTypes, lambda([ImportedType], member(ImportedType, ExportedTypes))),

        % get their corresponding data defs
        map(ImportedTypes,
            lambda([TypeName, Probe],
                   (Probe = defdata(TypeName, _, _),
                    member(Probe, DataDefs))),
            ImportedDataDefs),

        % get the corresponding constructors
        constructorsInDataDefs(ImportedDataDefs, Constructors).

clausedef(directLoadModule, [], [atom, % absolute filename
                                 list(loadedModule), % already loaded modules
                                 list(atom), % in progress loading
                                 list(loadedModule)]). % newly loaded modules
directLoadModule(FileName, AlreadyLoaded, InProgress,
                 [loadedModule(FileName, ModuleId, LoadedFile)|RestLoaded]) :-
        loadFile(FileName, LoadedFile),
        LoadedFile = loadedFile(_, UsesModules, DataDefs, _, _, _),

        % perform the actual loading
        foldLeft(UsesModules, AlreadyLoaded,
                 lambda([CurLoaded, def_use_module(CurFileName, _, _), TempLoaded],
                         loadModule(CurFileName, FileName, CurLoaded, InProgress, TempLoaded)),
                 RestLoaded),

        % ensure we haven't introduced any duplicate constructors
        allImportedConstructors(TempLoaded, FileName, UsesModules, ImportedConstructors),
        constructorsInDataDefs(DataDefs, LocalConstructors),
        append(ImportedConstructors, LocalConstructors, AllConstructors),
        is_set(AllConstructors),

        freshModuleId(ModuleId).

datadef(renaming, [], [renaming(list(pair(pair(atom, int), atom)), % for clauses
                                list(pair(atom, atom)), % for types
                                list(pair(atom, atom)))]). % for constructors

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
