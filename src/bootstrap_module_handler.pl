module(bootstrap_module_handler, [], []).

use_module('common.pl', [notMember/2, foldLeft/4, flatMap/3, map/3, forall/2,
                         foldRight/4],
                        [pair, tup3]).
use_module('bootstrap_syntax.pl', [loadFile/2],
                                  [op, exp, expLhs, term, bodyPairOp, body, type, defclause,
                                   typeConstructor, defdata, clauseclause, defglobalvar,
                                   defmodule, def_use_module, loadedFile]).

datadef(accessModifier, [], [mod_public, mod_private]).

clausedef(yolo_UNSAFE_mangled_name, [], [accessModifier, int, atom, atom]).
yolo_UNSAFE_mangled_name(AccessModifier, ModuleId, OriginalName, NewName) :-
        (AccessModifier == mod_public ->
            (WriteAccess = public);
            (WriteAccess = private)),
        format(atom(NewName), '~a_~d_~a', [WriteAccess, ModuleId, OriginalName]).

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
                                list(pair(atom, atom)), % for constructors
                                list(pair(atom, atom)))]). % for global variables
% assumes that there are no duplicates
clausedef(makeRenaming, [], [list(loadedModule), % all loaded modules
                             atom, % absolute filename of module of interest
                             renaming]).
makeRenaming(LoadedModules, Filename,
             renaming(ClauseRenaming, TypeRenaming, ConstructorRenaming, GlobalVarRenaming)) :-
        % get the corresponding module
        member(loadedModule(Filename, LocalModuleId, LoadedFile), LoadedModules),
        LoadedFile = loadedFile(defmodule(_, ExportedClauses, ExportedTypes),
                                UsesModules,
                                DataDefs,
                                ClauseDefs,
                                GlobalVarDefs,
                                _),
        
        % determine renamings for external clauses, types, and constructors
        foldRight(UsesModules, tup3([], [], []),
                  lambda([def_use_module(UsedFilename, ImportedClauses, ImportedTypes),
                          tup3(CurClauses, CurTypes, CurCons),
                          tup3(NewClauses, NewTypes, NewCons)],
                         (% determine the ID of the module
                          yolo_UNSAFE_absolute_file_name(UsedFilename, Filename, ExternalFilename),
                          member(loadedModule(ExternalFilename, ModuleId, _), LoadedModules),

                          % determine renamings for the imported clauses
                          map(ImportedClauses,
                              lambda([NameArity, pair(NameArity, NewClauseName)],
                                     (NameArity = pair(ClauseName, _),
                                      yolo_UNSAFE_mangled_name(mod_public, ModuleId,
                                                               ClauseName, NewClauseName))),
                              AddClauses),
                          append(AddClauses, CurClauses, NewClauses),

                          % determine renamings for the imported types
                          map(ImportedTypes,
                              lambda([TypeName, pair(TypeName, NewTypeName)],
                                     yolo_UNSAFE_mangled_name(mod_public, ModuleId,
                                                              TypeName, NewTypeName)),
                              AddTypes),
                          append(AddTypes, CurTypes, NewTypes),

                          % determine renamings for the imported constructors
                          importedConstructors(LoadedModules, ExternalFilename, ImportedTypes,
                                               ImportedConstructors),
                          map(ImportedConstructors,
                              lambda([ConsName, pair(ConsName, NewConsName)],
                                     yolo_UNSAFE_mangled_name(mod_public, ModuleId,
                                                              ConsName, NewConsName)),
                              AddCons),
                          append(AddCons, CurCons, NewCons))),
                  tup3(ExternalClauseRenaming, ExternalTypeRenaming, ExternalConstructorRenaming)),

        % determine renaming for local clause defs
        map(ClauseDefs,
            lambda([defclause(Name, _, Params), pair(pair(Name, Arity), NewName)],
                   (length(Params, Arity),
                    (member(pair(Name, Arity), ExportedClauses) ->
                        (AccessModifier = mod_public);
                        (AccessModifier = mod_private)),
                    yolo_UNSAFE_mangled_name(AccessModifier, LocalModuleId, Name, NewName))),
            LocalClauseRenaming),

        % determine renaming for local types and local constructors
        foldRight(DataDefs, pair([], []),
                  lambda([defdata(TypeName, _, TypeConstructors),
                          pair(CurTypes, CurCons),
                          pair([pair(TypeName, NewTypeName)|NewTypes], NewCons)],
                         (% determine the appropriate access modifier
                          (member(TypeName, ExportedTypes) ->
                              (AccessModifier = mod_public;
                               AccessModifier = mod_private)),

                           % determine the new type name
                           yolo_UNSAFE_mangled_name(AccessModifier, LocalModuleId,
                                                    TypeName, NewTypeName),
                           
                           % determine the new constructor names
                           map(TypeConstructors,
                               lambda([typeConstructor(ConsName, _), pair(ConsName, NewConsName)],
                                      yolo_UNSAFE_mangled_name(AccessModifier, LocalModuleId,
                                                               ConsName, NewConsName)),
                               AddCons),
                           append(AddCons, CurCons, NewCons))),
                  pair(LocalTypeRenaming, LocalConstructorRenaming)),

        % determine renaming for global variables
        map(GlobalVarDefs,
            lambda([defglobalvar(Name, _, _), pair(Name, NewName)],
                   yolo_UNSAFE_mangled_name(mod_private, LocalModuleId, Name, NewName)),
            GlobalVarRenaming),

        % put it all together
        append(LocalClauseRenaming, ExternalClauseRenaming, ClauseRenaming),
        append(LocalTypeRenaming, ExternalTypeRenaming, TypeRenaming),
        append(LocalConstructorRenaming, ExternalConstructorRenaming, ConstructorRenaming).

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
