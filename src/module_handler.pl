module(module_handler, [handleModules/5], []).

use_module('common.pl', [notMember/2, foldLeft/4, flatMap/3, map/3, forall/2,
                         foldRight/4, appendDiffList/3, onFailure/2, existsOnce/2,
                         yolo_UNSAFE_format_shim/2, duplicates/2, filter/3],
                        [pair, tup3, tup4, option]).
use_module('syntax.pl', [loadFile/2],
                        [op, exp, expLhs, term, body, type, defclause,
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

datadef(moduleUse, [], [moduleUse(int, % id of module used
                                  list(pair(atom, int)), % clauses used
                                  list(atom))]). % types used
datadef(loadedModule, [], [loadedModule(atom, % absolute filename
                                        int, % module ID
                                        list(moduleUse), % easier to process use_module directives
                                        loadedFile)]). % file corresponding to module

clausedef(yolo_UNSAFE_absolute_file_name, [], [atom, % name of file
                                               atom, % relative to another file
                                               atom]). % returned filename
yolo_UNSAFE_absolute_file_name(RelativeName, RelativeTo, AbsoluteName) :-
        absolute_file_name(RelativeName, AbsoluteName, [relative_to(RelativeTo)]),
        onFailure(
            lambda([], access_file(AbsoluteName, read)),
            lambda([], yolo_UNSAFE_format_shim(
                           'Could not read from possibly nonexistent file: ~w~n', [AbsoluteName]))).

clausedef(constructorsInDataDefs, [], [list(defdata), list(atom)]).
constructorsInDataDefs(DataDefs, Constructors) :-
        flatMap(DataDefs,
                lambda([defdata(_, _, TypeConstructors), CurConstructors],
                       map(TypeConstructors,
                           lambda([typeConstructor(Name, _), Name], true),
                           CurConstructors)),
                Constructors).

clausedef(allImportedConstructors, [], [list(loadedModule),
                                        list(moduleUse),
                                        list(atom)]).
allImportedConstructors(LoadedModules, UsesModules, Constructors) :-
        flatMap(UsesModules,
                lambda([moduleUse(ImportedId, _, ImportedTypes),
                        CurConstructors],
                       (LoadedModule = loadedModule(_, ImportedId, _, _),
                        member(LoadedModule, LoadedModules),
                        extractConstructors(LoadedModule, ImportedTypes, CurConstructors))),
                Constructors).

% Assumes that all dependencies have been loaded in.
clausedef(extractConstructors, [], [loadedModule, % module containing them
                                    list(atom),   % extract for these types
                                    list(atom)]). % resulting constructors
extractConstructors(loadedModule(_, _, _, LoadedFile),
                    ImportedTypes, Constructors) :-
        LoadedFile = loadedFile(defmodule(ModuleName, _, ExportedTypes), _, DataDefs, _, _, _),

        % ensure the module exports the types we want
        filter(ImportedTypes,
               lambda([ImportedType], notMember(ImportedType, ExportedTypes)),
               MissingTypes),
        onFailure(
                lambda([], MissingTypes = []),
                lambda([],
                       (yolo_UNSAFE_format_shim('Module ~w does not export the following types, which are imported elsewhere: ', [ModuleName]),
                        yolo_UNSAFE_format_shim('~w~n', [MissingTypes])))),

        % get their corresponding data defs
        map(ImportedTypes,
            lambda([TypeName, Probe],
                   (Probe = defdata(TypeName, _, _),
                    member(Probe, DataDefs))),
            ImportedDataDefs),

        % get the corresponding constructors
        constructorsInDataDefs(ImportedDataDefs, Constructors).

% Finds all exports that are not defined in the file.
clausedef(nonexistentExports, [], [loadedFile, % the file
                                   list(pair(atom, int)), % non-defined exported clauses
                                   list(atom)]).          % non-defined exported types
nonexistentExports(loadedFile(defmodule(_, ExportedClauses, ExportedTypes),
                              _,
                              DefinedTypes,
                              DefinedClauses,
                              _,
                              _),
                   NonexistentClauses,
                   NonexistentTypes) :-
    filter(ExportedClauses,
           lambda([pair(ClauseName, ClauseArity)],
                  \+ existsOnce(DefinedClauses,
                                lambda([defclause(ClauseName, _, Types)],
                                       length(Types, ClauseArity)))),
           NonexistentClauses),
    filter(ExportedTypes,
           lambda([TypeName],
                  \+ existsOnce(DefinedTypes,
                                lambda([defdata(TypeName, _, _)], true))),
           NonexistentTypes).

clausedef(ensureEverythingExportedIsDefined, [], [atom,         % filename
                                                  loadedFile]). % the file that was loaded in
ensureEverythingExportedIsDefined(FileName, LoadedFile) :-
    nonexistentExports(LoadedFile, NonexistentClauses, NonexistentTypes),
    onFailure(
            lambda([], (NonexistentClauses=[], NonexistentTypes=[])),
            lambda([], (
                       yolo_UNSAFE_format_shim('Something exported not defined in: ~w~n', [FileName]),
                       yolo_UNSAFE_format_shim('Nonexistent clauses are: ~w~n', [NonexistentClauses]),
                       yolo_UNSAFE_format_shim('Nonexistent types are: ~w~n', [NonexistentTypes])))).

clausedef(directLoadModule, [], [atom, % absolute filename
                                 list(loadedModule), % already loaded modules
                                 list(atom), % in progress loading
                                 list(loadedModule)]). % newly loaded modules
directLoadModule(FileName, AlreadyLoaded, InProgress,
                 [loadedModule(FileName, ModuleId, ProcessedUses, LoadedFile)|RestLoaded]) :-
        loadFile(FileName, LoadedFile),
        LoadedFile = loadedFile(_, UsesModules, DataDefs, _, _, _),

        % make sure everything exported actually exists
        ensureEverythingExportedIsDefined(FileName, LoadedFile),
        
        % perform the actual loading
        foldLeft(UsesModules, pair(AlreadyLoaded, ProcessedUses),
                 lambda([pair(CurLoaded,
                              [moduleUse(OtherId, ClausesUsed, TypesUsed)|RestUsed]),
                         def_use_module(CurFileName, ClausesUsed, TypesUsed),
                         pair(TempLoaded, RestUsed)],
                         (yolo_UNSAFE_absolute_file_name(CurFileName, FileName, AbsFileName),
                          loadModule(AbsFileName, CurLoaded, InProgress, TempLoaded),
                          member(loadedModule(AbsFileName, OtherId, _, _), TempLoaded))),
                 pair(RestLoaded, [])),

        % ensure we haven't introduced any duplicate constructors
        allImportedConstructors(RestLoaded, ProcessedUses, ImportedConstructors),
        constructorsInDataDefs(DataDefs, LocalConstructors),
        append(ImportedConstructors, LocalConstructors, AllConstructors),

        onFailure(
            lambda([], is_set(AllConstructors)),
            lambda([],
                   (duplicates(AllConstructors, DuplicateConstructors),
                    yolo_UNSAFE_format_shim('Duplicate constructors in scope: ~w~n', [DuplicateConstructors])))),

        freshModuleId(ModuleId).

clausedef(renamedClause, [], [renaming, atom, int, atom]).
renamedClause(renaming(Mapping, _, _, _), OldName, Arity, NewName) :-
        member(pair(pair(OldName, Arity), NewName), Mapping), !.
renamedClause(_, Name, _, Name). % TODO: this is a hack to allow us to escape to stock Prolog.

clausedef(renamedType, [], [renaming, atom, atom]).
renamedType(renaming(_, Mapping, _, _), OldName, NewName) :-
        member(pair(OldName, NewName), Mapping), !.
renamedType(_, Name, Name).

clausedef(renamedConstructor, [], [renaming, atom, atom]).
renamedConstructor(renaming(_, _, Mapping, _), OldName, NewName) :-
        member(pair(OldName, NewName), Mapping), !.
renamedConstructor(_, Name, Name).

clausedef(renamedGlobalVariable, [], [renaming, atom, atom]).
renamedGlobalVariable(renaming(_, _, _, Mapping), OldName, NewName) :-
        member(pair(OldName, NewName), Mapping), !.
renamedGlobalVariable(_, Name, Name).

datadef(renaming, [], [renaming(list(pair(pair(atom, int), atom)), % for clauses
                                list(pair(atom, atom)), % for types
                                list(pair(atom, atom)), % for constructors
                                list(pair(atom, atom)))]). % for global variables
% assumes that there are no duplicates
clausedef(makeRenaming, [], [list(loadedModule), % all loaded modules
                             loadedModule,
                             renaming]).
makeRenaming(LoadedModules, loadedModule(_, LocalModuleId, UsesModules, LoadedFile),
             renaming(ClauseRenaming, TypeRenaming, ConstructorRenaming, GlobalVarRenaming)) :-
        LoadedFile = loadedFile(defmodule(_, ExportedClauses, ExportedTypes),
                                _,
                                DataDefs,
                                ClauseDefs,
                                GlobalVarDefs,
                                _),

        % determine renamings for external clauses, types, and constructors
        foldRight(UsesModules, tup3([], [], []),
                  lambda([moduleUse(ModuleId, ImportedClauses, ImportedTypes),
                          tup3(CurClauses, CurTypes, CurCons),
                          tup3(NewClauses, NewTypes, NewCons)],
                         (% determine renamings for the imported clauses
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
                          LoadedModule = loadedModule(_, ModuleId, _, _),
                          member(LoadedModule, LoadedModules),
                          extractConstructors(LoadedModule, ImportedTypes, ImportedConstructors),
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
                          pair([pair(TypeName, NewTypeName)|CurTypes], NewCons)],
                         (% determine the appropriate access modifier
                          (member(TypeName, ExportedTypes) ->
                              (AccessModifier = mod_public);
                              (AccessModifier = mod_private)),

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

clausedef(loadModule, [], [atom,                 % absolute filename
                           list(loadedModule),   % already loaded modules
                           list(atom),           % modules whose loading is in progress
                           list(loadedModule)]). % newly loaded modules
loadModule(FileName, AlreadyLoaded, InProgress, NewLoaded) :-
        % don't allow cyclic loading, which would put us in an infinite loop
        notMember(FileName, InProgress),

        % if we've already loaded this module, we're done
        (member(loadedModule(FileName, _, _, _), AlreadyLoaded) ->
            (NewLoaded = AlreadyLoaded);
            (directLoadModule(FileName, AlreadyLoaded,
                              [FileName|InProgress],
                              NewLoaded))).

clausedef(translateVarUse, [], [renaming, atom, term, atom, term]).
translateVarUse(Renaming, VarUsed, Term, NewVarUsed, NewTerm) :-
        renamedGlobalVariable(Renaming, VarUsed, NewVarUsed),
        translateTerm(Renaming, Term, NewTerm).

clausedef(translateBody, [], [renaming, body, body]).
translateBody(_, body_is(Lhs, Exp), body_is(Lhs, Exp)).
translateBody(Renaming, body_setvar(VarName, Term), body_setvar(NewVarName, NewTerm)) :-
        translateVarUse(Renaming, VarName, Term, NewVarName, NewTerm).
translateBody(Renaming, body_getvar(VarName, Term), body_getvar(NewVarName, NewTerm)) :-
        translateVarUse(Renaming, VarName, Term, NewVarName, NewTerm).
translateBody(Renaming, bodyUnary(Op, Body), bodyUnary(Op, NewBody)) :-
        translateBody(Renaming, Body, NewBody).
translateBody(Renaming, bodyPair(B1, Op, B2), bodyPair(NewB1, Op, NewB2)) :-
        translateBody(Renaming, B1, NewB1),
        translateBody(Renaming, B2, NewB2).
translateBody(Renaming, higherOrderCall(What, Params), higherOrderCall(NewWhat, NewParams)) :-
        translateTerm(Renaming, What, NewWhat),
        translateTerms(Renaming, Params, NewParams).
translateBody(Renaming, firstOrderCall(Name, Params), firstOrderCall(NewName, NewParams)) :-
        length(Params, Arity),
        renamedClause(Renaming, Name, Arity, NewName),
        translateTerms(Renaming, Params, NewParams).

clausedef(translateTerms, [], [renaming, list(term), list(term)]).
translateTerms(Renaming, Terms, NewTerms) :-
        map(Terms, lambda([Term, NewTerm], translateTerm(Renaming, Term, NewTerm)), NewTerms).

clausedef(translateTerm, [], [renaming, term, term]).
translateTerm(_, term_var(Variable), term_var(Variable)).
translateTerm(_, term_num(N), term_num(N)).
translateTerm(Renaming, term_lambda(Params, Body), term_lambda(NewParams, NewBody)) :-
        translateTerms(Renaming, Params, NewParams),
        translateBody(Renaming, Body, NewBody).
translateTerm(Renaming,
              term_constructor(ConsName, Params),
              term_constructor(NewConsName, NewParams)) :-
        renamedConstructor(Renaming, ConsName, NewConsName),
        translateTerms(Renaming, Params, NewParams).

clausedef(translateTypes, [], [renaming, list(type), list(type)]).
translateTypes(Renaming, Types, NewTypes) :-
        map(Types, lambda([Type, NewType], translateType(Renaming, Type, NewType)), NewTypes).

clausedef(translateType, [], [renaming, type, type]).
translateType(_, Variable, NewVariable) :-
        var(Variable),
        !,
        Variable = NewVariable.
translateType(_, intType, intType) :- !.
translateType(_, atomType, atomType) :- !.
translateType(Renaming, relationType(Types), relationType(NewTypes)) :-
        !,
        translateTypes(Renaming, Types, NewTypes).
translateType(Renaming, constructorType(Name, Types), constructorType(NewName, NewTypes)) :-
        !,
        renamedType(Renaming, Name, NewName),
        translateTypes(Renaming, Types, NewTypes).

clausedef(translateDataDef, [], [renaming, defdata, defdata]).
translateDataDef(Renaming,
                 defdata(Name, TypeParams, Constructors),
                 defdata(NewName, TypeParams, NewConstructors)) :-
        renamedType(Renaming, Name, NewName),
        map(Constructors,
            lambda([typeConstructor(ConstructorName, Types),
                    typeConstructor(NewConstructorName, NewTypes)],
                   (renamedConstructor(Renaming, ConstructorName, NewConstructorName),
                    translateTypes(Renaming, Types, NewTypes))),
            NewConstructors).

clausedef(translateLoadedFile, [], [renaming,
                                    loadedFile,
                                    list(defdata), % diff list
                                    list(defdata),
                                    list(defclause), % diff list
                                    list(defclause),
                                    list(defglobalvar), % diff list
                                    list(defglobalvar),
                                    list(clauseclause), % diff list
                                    list(clauseclause)]).
translateLoadedFile(Renaming,
                    loadedFile(_, _, DataDefs, ClauseDefs, GlobalVarDefs, Clauses),
                    DataDefInput, DataDefOutput,
                    ClauseDefInput, ClauseDefOutput,
                    GlobalVarInput, GlobalVarOutput,
                    ClauseInput, ClauseOutput) :-
        % handle the data defs
        map(DataDefs,
            lambda([DataDef, NewDataDef], translateDataDef(Renaming, DataDef, NewDataDef)),
            NewDataDefs),
        appendDiffList(NewDataDefs, DataDefInput, DataDefOutput),

        % handle the clause defs
        map(ClauseDefs,
            lambda([defclause(Name, TypeParams, Types),
                    defclause(NewName, TypeParams, NewTypes)],
                   (length(Types, Arity),
                    renamedClause(Renaming, Name, Arity, NewName),
                    translateTypes(Renaming, Types, NewTypes))),
            NewClauseDefs),
        appendDiffList(NewClauseDefs, ClauseDefInput, ClauseDefOutput),

        % handle the global var defs
        map(GlobalVarDefs,
            lambda([defglobalvar(Name, TypeParams, Type),
                    defglobalvar(NewName, TypeParams, NewType)],
                   (renamedGlobalVariable(Renaming, Name, NewName),
                    translateType(Renaming, Type, NewType))),
            NewGlobalVarDefs),
        appendDiffList(NewGlobalVarDefs, GlobalVarInput, GlobalVarOutput),

        % handle the clauses
        map(Clauses,
            lambda([clauseclause(Name, Params, Body),
                    clauseclause(NewName, NewParams, NewBody)],
                   (length(Params, Arity),
                    renamedClause(Renaming, Name, Arity, NewName),
                    translateTerms(Renaming, Params, NewParams),
                    translateBody(Renaming, Body, NewBody))),
            NewClauses),
        appendDiffList(NewClauses, ClauseInput, ClauseOutput).

clausedef(translateModule, [], [list(loadedModule), % all loaded modules
                                loadedModule, % what to translate
                                list(defdata), % diff list
                                list(defdata),
                                list(defclause), % diff list
                                list(defclause),
                                list(defglobalvar), % diff list
                                list(defglobalvar),
                                list(clauseclause), % diff list
                                list(clauseclause)]).
translateModule(AllModules, LoadedModule,
                DataDefInput, DataDefOutput,
                ClauseDefInput, ClauseDefOutput,
                GlobalVarInput, GlobalVarOutput,
                ClauseInput, ClauseOutput) :-
        LoadedModule = loadedModule(_, _, _, LoadedFile),
        makeRenaming(AllModules, LoadedModule, Renaming),
        translateLoadedFile(Renaming, LoadedFile,
                            DataDefInput, DataDefOutput,
                            ClauseDefInput, ClauseDefOutput,
                            GlobalVarInput, GlobalVarOutput,
                            ClauseInput, ClauseOutput), !.

clausedef(handleModules, [], [atom, % Entry point possibly relative filename
                              list(defdata), list(defclause),
                              list(defglobalvar), list(clauseclause)]).
handleModules(Filename, DataDefs, ClauseDefs, GlobalVarDefs, Clauses) :-
        setvar(counter, 0),
        yolo_UNSAFE_absolute_file_name(Filename, './', AbsFilename),
        directLoadModule(AbsFilename, [], [AbsFilename], LoadedModules), !,
        foldLeft(LoadedModules, tup4(DataDefs, ClauseDefs, GlobalVarDefs, Clauses),
                 lambda([tup4(CurDataDefs, CurClauseDefs, CurGlobalVarDefs, CurClauses),
                         LoadedModule,
                         tup4(NewDataDefs, NewClauseDefs, NewGlobalVarDefs, NewClauses)],
                        translateModule(LoadedModules, LoadedModule,
                                        CurDataDefs, NewDataDefs,
                                        CurClauseDefs, NewClauseDefs,
                                        CurGlobalVarDefs, NewGlobalVarDefs,
                                        CurClauses, NewClauses)),
                 tup4([], [], [], [])).
