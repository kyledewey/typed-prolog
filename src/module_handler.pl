:- module('module_handler', [handleModules/5]).

:- use_module('clauses_util.pl', [loadFile/2]).
:- use_module('util.pl').

% loadedModule(AbsoluteFileName, ModuleId, loadedFile)

% -RelativeFileName
% -AbsoluteFileName
%
% Gets the absolute file name of a file.  Also ensures
% that the file exists and we can read it; fails if it doesn't.
absoluteFileName(RelativeFileName, AbsoluteFileName) :-
        absolute_file_name(RelativeFileName, AbsoluteFileName),
        access_file(AbsoluteFileName, read).

% -AccessModifier:  public | private
% -CurrentModuleId: Int
% -OriginalName:    Atom
% -NewName:         Atom
mangledName(AccessModifier, ModuleId, OriginalName, NewName) :-
        format(atom(NewName), '~a_~d_~a', [AccessModifier, ModuleId, OriginalName]).

% -ClauseDefs: [ClauseDef]
% -Name:       Name
% -Arity:      Int
% -ClauseDef:  ClauseDef
clausedefWithNameArity(ClauseDefs, Name, Arity, ClauseDef) :-
        ClauseDef = clausedef(Name, _, Params),
        member(ClauseDef, ClauseDefs),
        length(Params, Arity).

% -[A]
% -A
member_(List, Probe) :-
        member(Probe, List).

% -DataDef: DataDef
% -Name
datadefTypeName(datadef(Name, _, _), Name).

% -Name
% -DataDef: DataDef
datadefTypeName_(Name, DataDef) :-
        datadefTypeName(DataDef, Name).

% -AllModules:      [loadedModule]
% -ModulesUsed:     [UseModule]
% -ConstructorName: Atom
% -Module:          loadedModule
moduleThatExportsConstructor(
        AllModules,
        [use_module(FileName, _, ExportedData)|_],
        ConstructorName, Module) :-
        
        % get the loaded version of the file
        Module = loadedModule(AbsoluteFileName, _, LoadedFile),
        absoluteFileName(FileName, AbsoluteFileName),
        member(Module, AllModules),

        % ensure that the file actually exported the things claimed,
        % which includes what we're looking for
        LoadedFile = loadedFile(DataDefs, _, _, some(module(_, _, ActualExports)), _, _),
        maplist(member_(ActualExports), ExportedData),

        % see if any of the exported datadefs has the constructor we are looking for
        member(ExportedDataDefNameProbe, ExportedData),
        ProbeDataDef = datadef(ExportedDataDefNameProbe, _, _),
        member(ProbeDataDef, DataDefs),
        datadefConstructorNames(DataDefs, Names),
        member(ConstructorName, Names).
moduleThatExportsConstructor(AllModules, [_|T], ConstructorName, Module) :-
        moduleThatExportsConstructor(AllModules, T, ConstructorName, Module).
        
% -AllModules:  [loadedModule]
% -ModulesUsed: [UseModule]
% -ClauseName:  Atom
% -Arity:       Int
% -Module:      loadedModule
moduleThatExportsClause(
        AllModules,
        [use_module(FileName, ExportedClauses, _)|_],
        ClauseName, Arity, Module) :-
        % ensure that we marked this as being in another file
        member(/(ClauseName, Arity), ExportedClauses),

        % get the file that it's supposed to be in
        absoluteFileName(FileName, AbsoluteFileName),
        Module = loadedModule(AbsoluteFileName, _, LoadedFile),
        member(Module, AllModules),

        % ensure that the file actually exports everything we are pulling in,
        % which includes the target clause.
        LoadedFile = loadedFile(_, _, _, option(module(_, ActuallyExports, _)), _, _),
        maplist(member_(ActuallyExports), ExportedClauses).
moduleThatExportsClause(AllModules, [_|T], ClauseName, Arity, Module) :-
        moduleThatExportsClause(AllModules, T, ClauseName, Arity, Module).

% -CurrentModuleId: Int
% -CurrentFile:     loadedFile
% -Modules:         [loadedModule]
% -InputName
% -Arity
% -OutputName
%
% This is from the standpoint of the current module.
translateCurrentClauseName(
        ModuleId, 
        loadedFile(_, ClauseDefs, _, some(module(_, ExportedClauses, _)), ModulesUsed, _),
        Modules, InputName, Arity, TranslatedName) :-

        % Valid possibilities:
        % -We are calling something private to this module
        % -We are calling something exported by this module
        % -We are calling something exported by another module

        % Is the called clause defined in this module?
        (clausedefWithNameArity(ClauseDefs, InputName, Arity, _) ->
            % Is the clause exported?
            ((member(/(InputName, Arity), ExportedClauses) ->
                (AccessModifier = public);
                (AccessModifier = private)),
             mangledName(AccessModifier, ModuleId, InputName, TranslatedName));
            (moduleThatExportsClause(Modules, ModulesUsed, InputName, Arity,
                                     loadedModule(_, OtherModuleId, _)),
             mangledName(public, OtherModuleId, InputName, TranslatedName))).

% -Alternative
% -Name: Atom
constructorName(Alternative, Name) :-
        Alternative =.. [Name|_].

% -DataDef: DataDef
% -Names:   [Atom]
datadefConstructorNames(datadef(_, _, Alternatives), Names) :-
        maplist(constructorName, Alternatives, Names).

% -DataDefs:    [DataDef]
% -Constructor: Atom
% -DataDef:     DataDef
datadefWithConstructor([H|_], ConstructorName, H) :-
        datadefConstructorNames(H, Names),
        member(ConstructorName, Names).
datadefWithConstructor([_|T], ConstructorName, Result) :-
        datadefWithConstructor(T, ConstructorName, Result).

% -CurrentModuleId: Int
% -CurrentFile:     loadedFile
% -Modules:         [loadedModule]
% -ConstructorName
% -OutputName
translatedUsedConstructorName(
        ModuleId,
        loadedFile(DataDefs, _, _, some(module(_, _, ExportedData)), ModulesUsed, _),
        Modules, ConstructorName, TranslatedName) :-
        
        % Valid possibilities:
        % -We are using something private to this module
        % -We are using something exported by this module
        % -We are using something exported by another module
        
        % Is the used constructor defined in this module?
        (datadefWithConstructor(DataDefs, ConstructorName, datadef(TypeName, _, _)) ->
            % Is the type corresponding to the constructor exported?
            ((member(TypeName, ExportedData) ->
                (AccessModifier = public);
                (AccessModifier = private)),
             mangledName(AccessModifier, ModuleId, ConstructorName, TranslatedName));
            (moduleThatExportsConstructor(Modules, ModulesUsed, ConstructorName,
                                          loadedModule(_, OtherModuleId, _)),
             mangledName(public, OtherModuleId, ConstructorName, TranslatedName))).

% -TypeMapping: [pair(OrigName, MangledName)]
% -InputTypes:  [Type]
% -OutputTypes: [Type]
translateTypes(TypeMapping, InputTypes, OutputTypes) :-
        maplist(translateType(TypeMapping), InputTypes, OutputTypes).

% -TypeMapping: [pair(OrigName, MangledName)]
% -InputType:  Type
% -OutputType: Type
translateType(_, InputTypeVar, OutputTypeVar) :-
        var(InputTypeVar),
        !,
        InputTypeVar = OutputTypeVar.
translateType(_, int, int) :- !.
translateType(TypeMapping, relation(Types), relation(NewTypes)) :-
        !,
        translateTypes(TypeMapping, Types, NewTypes).
translateType(TypeMapping, ConstructorType, NewConstructorType) :-
        ConstructorType =.. [OrigName|OrigTypes],
        !,
        member(pair(OrigName, NewName), TypeMapping),
        translateTypes(TypeMapping, OrigTypes, NewTypes),
        NewConstructorType =.. [NewName|NewTypes].

% -AllModules:  [LoadedModule]
% -ModuleUse:   UseModule
% -TypeMapping: [pair(OrigName, MangledName)]
makeTypeMappingForModuleUse(AllModules,
                            use_module(FileName, _, PullInThisData),
                            TypeMapping) :-
        absoluteFileName(FileName, AbsoluteFileName),
        member(loadedModule(AbsoluteFileName, ModuleId, _), AllModules),
        maplist(mangledNamePair(public, ModuleId), PullInThisData, TypeMapping).


% -AllModules:  [LoadedModule]
% -ModuleUsed:  [UseModule]
% -TypeMapping: [pair(OrigName, MangledName)]
makeTypeMappingForModulesUsed(_, [], []).
makeTypeMappingForModulesUsed(AllModules, [H|T], Result) :-
        makeTypeMappingForModuleUse(AllModules, H, Current),
        makeTypeMappingForModulesUsed(AllModules, T, Rest),
        append(Current, Rest, Result).

% -AccessModifier:  public | private
% -ModuleId:        Int
% -OriginalName:    Atom
% -Pair:            pair(OriginalName, NewName)
mangledNamePair(AccessModifier, ModuleId, OriginalName, pair(OriginalName, MangledName)) :-
        mangledName(AccessModifier, ModuleId, OriginalName, MangledName).

% -AllModules:  [LoadedModule]
% -Module:      LoadedModule
% -TypeMapping: [pair(OrigName, MangledName)]
makeTypeMapping(AllModules,
                loadedModule(_, ModuleId, LoadedFile),
                TypeMapping) :-
        LoadedFile = loadedFile(DataDefs, _, _, some(module(_, _, MyExports)),
                                ModulesUsed, _),
        % grab everything we declared to be public                                
        maplist(mangledNamePair(public, ModuleId), MyExports, Public),
        
        % grab everything we declared to be private
        maplist(datadefTypeName, DataDefs, DataDefNames),
        diffEqual(DataDefNames, MyExports, PrivateNames),
        maplist(mangledNamePair(private, ModuleId), PrivateNames, Private),

        % grab everything we pulled in from elsewhere
        makeTypeMappingForModulesUsed(AllModules, ModulesUsed, RestMangledExports),

        % put it all together
        append(Public, Private, MyTypeMapping),
        append(MyTypeMapping, RestMangledExports, TypeMapping).

% -TypeMapping:           [pair(OrigName, MangledName)]
% -ModuleId:              Int
% -AccessModifier:        public | private
% -Alternative:           Alternative
% -TranslatedAlternative: Alternative
translateDataDefAlternative(TypeMapping, ModuleId, AccessModifier,
                            Alternative, TranslatedAlternative) :-
        Alternative =.. [OrigName|Types],
        mangledName(AccessModifier, ModuleId, OrigName, NewName),
        translateTypes(TypeMapping, Types, NewTypes),
        TranslatedAlternative =.. [NewName|NewTypes].

% -TypeMapping:            [pair(OrigName, MangledName)]
% -ModuleId:               Int
% -AccessModifier:         public | private
% -Alternatives:           [Alternative]
% -TranslatedAlternatives: [Alternative]
translateDataDefAlternatives(TypeMapping, ModuleId, AccessModifier,
                             Alternatives, TranslatedAlternatives) :-
        maplist(translateDataDefAlternative(TypeMapping, ModuleId, AccessModifier),
                Alternatives, TranslatedAlternatives).

% -TypeMapping:        [pair(OrigName, MangledName)]
% -ModuleId:           Int
% -ExportedDataTypes:  [Name]
% -DataDef:            DataDef
% -TranslatedDataDef:  DataDef
translateDataDef(TypeMapping, ModuleId, ExportedDataTypes,
                 datadef(OrigName, TypeParams, Alternatives),
                 datadef(MangledName, TypeParams, TranslatedAlternatives)) :-
        % has the data type been exported?
        (member(OrigName, ExportedDataTypes) ->
            (AccessModifier = public);
            (AccessModifier = private)),
        mangledName(AccessModifier, ModuleId, OrigName, MangledName),
        translateDataDefAlternatives(TypeMapping, ModuleId, AccessModifier,
                                     Alternatives, TranslatedAlternatives).
        
% -TypeMapping:        [pair(OrigName, MangledName)]
% -ModuleId:           Int
% -ExportedDataTypes:  [Name]
% -DataDefs:           [DataDef]
% -TranslatedDataDefs: [DataDef]
translateDataDefs(TypeMapping, ModuleId, ExportedDataTypes,
                  DataDefs, TranslatedDataDefs) :-
        maplist(translateDataDef(TypeMapping, ModuleId, ExportedDataTypes),
                DataDefs, TranslatedDataDefs).

% -TypeMapping:         [pair(OrigName, MangledName)]
% -ModuleId:            Int
% -ExportedClauses:     [Name/Arity]
% -ClauseDef:           ClauseDef
% -TranslatedClauseDef: ClauseDef
translateClauseDef(TypeMapping, ModuleId, ExportedClauses,
                   clausedef(OrigName, TypeParams, Types),
                   clausedef(MangledName, TypeParams, TranslatedTypes)) :-
        % has the clause been exported?
        length(Types, Arity),
        (member(/(OrigName, Arity), ExportedClauses) ->
            (AccessModifier = public);
            (AccessModifier = private)),
        mangledName(AccessModifier, ModuleId, OrigName, MangledName),
        translateTypes(TypeMapping, Types, TranslatedTypes).

% -TypeMapping:          [pair(OrigName, MangledName)]
% -ModuleId:             Int
% -ExportedClauses:      [Name/Arity]
% -ClauseDefs:           [ClauseDef]
% -TranslatedClauseDefs: [ClauseDef]
translateClauseDefs(TypeMapping, ModuleId, ExportedClauses,
                    ClauseDefs, TranslatedClauseDefs) :-
        maplist(translateClauseDef(TypeMapping, ModuleId, ExportedClauses),
                ClauseDefs, TranslatedClauseDefs).

% -TypeMapping:            [pair(OrigName, MangledName)]
% -ModuleId:               Int
% -GlobalVarDef:           GlobalVarDef
% -TranslatedGlobalVarDef: GlobalVarDef
translateGlobalVarDef(TypeMapping, ModuleId,
                      globalvardef(OrigName, TypeParams, Type),
                      globalvardef(MangledName, TypeParams, TranslatedType)) :-
        mangledName(private, ModuleId, OrigName, MangledName),
        translateType(TypeMapping, Type, TranslatedType).

% -TypeMapping:             [pair(OrigName, MangledName)]
% -ModuleId:                Int
% -GlobalVarDefs:           [GlobalVarDef]
% -TranslatedGlobalVarDefs: [GlobalVarDef]
translateGlobalVarDefs(TypeMapping, ModuleId, GlobalVarDefs,
                       TranslatedGlobalVarDefs) :-
        maplist(translateGlobalVarDef(TypeMapping, ModuleId),
                GlobalVarDefs, TranslatedGlobalVarDefs).

% -AllModules:     [LoadedModule]
% -ModuleId:       Int
% -CurrentFile:    loadedFile
% -Body:           Body
% -TranslatedBody: Body
translateBody(_, _, _, AtomForm, AtomForm) :-
        bodyAtomForm(AtomForm),
        !.
translateBody(_, _, _, Is, Is) :-
        Is = is(_, _),
        !.
translateBody(AllModules, ModuleId, CurrentFile,
              VarForm, TranslatedVarForm) :-
        bodyVarForm(VarForm, VarOpName, VarName, Term),
        !,
        mangledName(private, ModuleId, VarName, MangledVarName),
        translateTerm(AllModules, ModuleId, CurrentFile,
                      Term, TranslatedTerm),
        TranslatedVarForm =.. [VarOpName, MangledVarName, TranslatedTerm].
translateBody(AllModules, ModuleId, CurrentFile,
              PairForm, TranslatedPairForm) :-
        bodyPairForm(PairForm, PairOpName, B1, B2),
        !,
        translateBody(AllModules, ModuleId, CurrentFile, B1, TranslatedB1),
        translateBody(AllModules, ModuleId, CurrentFile, B2, TranslatedB2),
        TranslatedPairForm =.. [PairOpName, TranslatedB1, TranslatedB2].
translateBody(AllModules, ModuleId, CurrentFile,
              HigherOrderCall, TranslatedHigherOrderCall) :-
        HigherOrderCall =.. [call|Params],
        !,
        translateTerms(AllModules, ModuleId, CurrentFile,
                       Params, TranslatedParams),
        TranslatedHigherOrderCall =.. [call|TranslatedParams].
translateBody(AllModules, ModuleId, CurrentFile,
              FirstOrderCall, TranslatedFirstOrderCall) :-
        FirstOrderCall =.. [Name|Params],
        !,
        length(Params, Arity),
        translateCurrentClauseName(ModuleId, CurrentFile, AllModules,
                                   Name, Arity, MangledName),
        translateTerms(AllModules, ModuleId, CurrentFile,
                       Params, TranslatedParams),
        TranslatedFirstOrderCall =.. [MangledName|TranslatedParams].

% -AllModules:      [LoadedModule]
% -ModuleId:        Int
% -CurrentFile:     loadedFile
% -Terms:           [Term]
% -TranslatedTerms: [Term]
translateTerms(AllModules, ModuleId, CurrentFile,
               Terms, TranslatedTerms) :-
        maplist(translateTerm(AllModules, ModuleId, CurrentFile),
                Terms, TranslatedTerms).

% -AllModules:     [LoadedModule]
% -ModuleId:       Int
% -CurrentFile:    loadedFile
% -Term:           Term
% -TranslatedTerm: Term
translateTerm(_, _, _, Var, NewVar) :-
        var(Var),
        !,
        Var = NewVar.
translateTerm(AllModules, ModuleId, CurrentFile,
              Atom, NewAtom) :-
        atom(Atom),
        !,
        translateUsedConstructorName(ModuleId, CurrentFile, AllModules,
                                     Atom, NewAtom).
translateTerm(_, _, _, Int, Int) :-
        number(Int),
        !.
translateTerm(AllModules, ModuleId, CurrentFile,
              lambda(Params, Body),
              lambda(NewParams, NewBody)) :-
        !,
        translateTerms(AllModules, ModuleId, CurrentFile, Params, NewParams),
        translateBody(AllModules, ModuleId, CurrentFile, Body, NewBody).
translateTerm(AllModules, ModuleId, CurrentFile,
              Structure, TranslatedStructure) :-
        Structure =.. [Name|Params],
        !,
        translateUsedConstructor(ModuleId, CurrentFile, AllModules,
                                 Name, NewName),
        translateTerms(AllModules, ModuleId, CurrentFile, Params, NewParams),
        TranslatedStructure =.. [NewName|NewParams].

% -AllModules:       [LoadedModule]
% -ModuleId:         Int
% -CurrentFile:      loadedFile
% -Clause:           Clause
% -TranslatedClause: Clause
translateClause(AllModules, ModuleId, CurrentFile,
                :-(OrigHead, OrigBody),
                :-(NewHead, NewBody)) :-
        OrigHead =.. [OrigName|OrigParams],
        length(OrigParams, Arity),
        translateCurrentClauseName(ModuleId, CurrentFile, AllModules,
                                   OrigName, Arity, MangledName),
        translateTerms(AllModules, ModuleId, CurrentFile, OrigParams, NewParams),
        translateBody(AllModules, ModuleId, CurrentFile, OrigBody, NewBody),
        NewHead =.. [MangledName|NewParams].

% -AllModules:        [LoadedModule]
% -ModuleId:          Int
% -CurrentFile:       loadedFile
% -Clauses:           [Clause]
% -TranslatedClauses: [Clause]
translateClauses(AllModules, ModuleId, CurrentFile,
                 Clauses, TranslatedClauses) :-
        maplist(translateClause(AllModules, ModuleId, CurrentFile),
                Clauses, TranslatedClauses).

% -AllModules:              [LoadedModule]
% -TranslatedDataDefs:      [DataDef]
% -TranslatedClauseDefs:    [ClauseDef]
% -TranslatedGlobalVarDefs: [GlobalVarDef]
% -TranslatedClauses:       [Clause]
translateModules(AllModules,
                 TranslatedDataDefs, TranslatedClauseDefs,
                 TranslatedGlobalVarDefs, TranslatedClauses) :-
        translateModules(AllModules, AllModules,
                         TranslatedDataDefs, TranslatedClauseDefs,
                         TranslatedGlobalVarDefs, TranslatedClauses).

% -AllModules:              [LoadedModule]
% -ModuleBreakdown:         [LoadedModule]
% -TranslatedDataDefs:      [DataDef]
% -TranslatedClauseDefs:    [ClauseDef]
% -TranslatedGlobalVarDefs: [GlobalVarDef]
% -TranslatedClauses:       [Clause]
%
% TODO: we should be using difference lists here for performance.
translateModules(_, [], [], [], [], []).
translateModules(AllModules, [H|T],
                 TranslatedDataDefs, TranslatedClauseDefs,
                 TranslatedGlobalVarDefs, TranslatedClauses) :-
        translateModule(AllModules, H,
                        TempDataDefs, TempClauseDefs,
                        TempGlobalVarDefs, TempClauses),
        translateModules(AllModules, T,
                         RestDataDefs, RestClauseDefs,
                         RestGlobalVarDefs, RestClauses),
        append(TempDataDefs, RestDataDefs, TranslatedDataDefs),
        append(TempClauseDefs, RestClauseDefs, TranslatedClauseDefs),
        append(TempGlobalVarDefs, RestGlobalVarDefs, TranslatedGlobalVarDefs),
        append(TempClauses, RestClauses, TranslatedClauses).

% -AllModules:              [LoadedModule]
% -ModuleToTranslate:       LoadedModule
% -TranslatedDataDefs:      [DataDef]
% -TranslatedClauseDefs:    [ClauseDef]
% -TranslatedGlobalVarDefs: [GlobalVarDef]
% -TranslatedClauses:       [Clause]
translateModule(AllModules, Module, TranslatedDataDefs, TranslatedClauseDefs,
                TranslatedGlobalVarDefs, TranslatedClauses) :-
        % extract out subcomponents
        Module = loadedModule(_, ModuleId, LoadedFile),
        LoadedFile = loadedFile(DataDefs, ClauseDefs, GlobalVarDefs, ModuleDef, _, Clauses),
        ModuleDef = some(module(_, ExportedClauses, ExportedDataTypes)),

        % translate the data defs
        makeTypeMapping(AllModules, Module, TypeMapping),
        translateDataDefs(TypeMapping, ModuleId, ExportedDataTypes,
                          DataDefs, TranslatedDataDefs),
        
        % translate the clause defs
        translateClauseDefs(TypeMapping, ModuleId, ExportedClauses,
                            ClauseDefs, TranslatedClauseDefs),

        % translate the global variable defs
        translateGlobalVarDefs(TypeMapping, ModuleId, GlobalVarDefs,
                               TranslatedGlobalVarDefs),

        % translate the clauses
        translateClauses(AllModules, ModuleId, LoadedFile,
                         Clauses, TranslatedClauses).

% -ModuleUse:       UseModule
% -AlreadyLoaded:   [loadedModule]
% -PartiallyLoaded: [AbsoluteFileName]
% -NewLoaded:       [loadedModule]
loadModuleFromModuleUse(use_module(Filename, _, _),
                        AlreadyLoaded, PartiallyLoaded, NewLoaded) :-
        loadModule(Filename, AlreadyLoaded, PartiallyLoaded, NewLoaded).

% -ModuleUsed:         [UseModule]
% -AlreadyLoaded:      [loadedModule]
% -PartiallyLoaded:    [AbsoluteFileName]
% -NewLoaded:          [loadedModule]
loadModulesFromModulesUsed([], Loaded, _, Loaded).
loadModulesFromModulesUsed([H|T], AlreadyLoaded, PartiallyLoaded, NewLoaded) :-
        loadModuleFromModuleUse(H, AlreadyLoaded, PartiallyLoaded, TempLoaded),
        loadModulesFromModulesUsed(T, TempLoaded, PartiallyLoaded, NewLoaded).

% -Id: Int
freshModuleId(N) :-
        nb_getval(module_counter, N),
        NewN is N + 1,
        nb_setval(module_counter, NewN).

% -FileName:        AbsoluteFilename
% -AlreadyLoaded:   [loadedModule]
% -PartiallyLoaded: [AbsoluteFileName]
% -NewLoaded:       [loadedModule]
%
% Skips checking partially loaded and already loaded.
directLoadModule(FileName, AlreadyLoaded, PartiallyLoaded, NewLoaded) :-
        loadFile(FileName, LoadedFile),
        LoadedFile = loadedFile(_, _, _, _, UsesModules, _),
        loadModulesFromModulesUsed(UsesModules, AlreadyLoaded, PartiallyLoaded, Rest),
        freshModuleId(ModuleId),
        NewLoaded = [loadedModule(FileName, ModuleId, LoadedFile)|Rest].

% -FileName:           PossiblyRelativeFileName
% -AlreadyLoaded:      [loadedModule]
% -PartiallyLoaded:    [AbsoluteFileName]
% -NewLoaded:          [loadedModule]
loadModule(RelativeFileName, AlreadyLoaded, PartiallyLoaded, NewLoaded) :-
        absoluteFileName(RelativeFileName, AbsoluteFileName),

        % don't allow cyclic loading, which would put us in an infinite loop
        \+ member(AbsoluteFileName, PartiallyLoaded),

        % if we've already loaded this module, we're done
        (member(loadedModule(AbsoluteFileName, _, _), AlreadyLoaded) ->
            (NewLoaded = AlreadyLoaded);
            (directLoadModule(AbsoluteFileName, AlreadyLoaded,
                              [AbsoluteFileName|PartiallyLoaded],
                              NewLoaded))).

% -EntryPoint: FileName
% -TranslatedDataDefs:      [DataDef]
% -TranslatedClauseDefs:    [ClauseDef]
% -TranslatedGlobalVarDefs: [GlobalVarDef]
% -TranslatedClauses:       [Clause]
handleModules(FileName,
              TranslatedDataDefs, TranslatedClauseDefs,
              TranslatedGlobalVarDefs, TranslatedClauses) :-
        loadModule(FileName, [], [], LoadedModules),
        translateModules(LoadedModules,
                         TranslatedDataDefs, TranslatedClauseDefs,
                         TranslatedGlobalVarDefs, TranslatedClauses).
