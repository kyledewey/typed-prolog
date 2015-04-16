:-use_module(library(clpfd)).
call_lambda0(lambda0_68(A)):-public_0_yolo_UNSAFE_format_shim('Syntax error in clause: ~w~n',[A]).
call_lambda0(lambda0_67(B,A)):-private_2_yolo_UNSAFE_translate_clause(A,B).
call_lambda0(lambda0_66(A)):-public_0_yolo_UNSAFE_format_shim('Syntax error in type: ~w~n',[A]).
call_lambda0(lambda0_65(B,C,A)):-private_2_yolo_UNSAFE_translate_type(A,B,C).
call_lambda0(lambda0_63(A)):-public_0_yolo_UNSAFE_format_shim('Syntax error in term: ~w~n',[A]).
call_lambda0(lambda0_62(B,A)):-private_2_yolo_UNSAFE_translate_term(A,B).
call_lambda0(lambda0_60(A)):-public_0_yolo_UNSAFE_format_shim('Syntax error in body: ~w~n',[A]).
call_lambda0(lambda0_59(B,A)):-private_2_yolo_UNSAFE_translate_body(A,B).
call_lambda0(lambda0_39(A)):-public_0_duplicates(A,B),public_0_yolo_UNSAFE_format_shim('Duplicate constructors in scope: ~w~n',[B]).
call_lambda0(lambda0_38(A)):-is_set(A).
call_lambda0(lambda0_31(A)):-public_0_yolo_UNSAFE_format_shim('Could not read from possibly nonexistent file: ~w~n',[A]).
call_lambda0(lambda0_30(A)):-access_file(A,read).
call_lambda0(lambda0_26(D,C,B,A)):-public_0_yolo_UNSAFE_format_shim('Type error at term ~w~n',[A]),public_0_onFailure(lambda0_24(B,C,A,D),lambda0_25(B)).
call_lambda0(lambda0_25(A)):-public_0_yolo_UNSAFE_format_shim('\tFound: UNKNOWN~n\tExpected: ~w~n',[A]).
call_lambda0(lambda0_24(E,A,C,B)):-private_4_rawTypeofTerm(A,B,C,D,_),public_0_yolo_UNSAFE_format_shim('\tFound: ~w~n\tExpected: ~w~n',[D,E]).
call_lambda0(lambda0_23(B,D,E,C,A)):-private_4_rawTypeofTerm(A,B,C,D,E).
call_lambda0(lambda0_22(A,B)):-public_0_yolo_UNSAFE_format_shim('Unknown clause: ~w~n',[public_0_pair(A,B)]).
call_lambda0(lambda0_21(B,D,C,A)):-private_4_expectedFormalParamTypes(A,B,C,D).
call_lambda0(lambda0_20(A)):-public_0_yolo_UNSAFE_format_shim('Type error at body ~w~n',[A]).
call_lambda0(lambda0_19(B,D,C,A)):-private_4_rawTypecheckBody(A,B,C,D).
call_lambda1(lambda1_77(A),B):-A\=B.
call_lambda1(lambda1_76(A),B):-public_0_setContains(A,B).
call_lambda1(lambda1_74(A),B):-public_0_forall(A,lambda1_73(B)).
call_lambda1(lambda1_73(B),A):-private_1_yolo_UNSAFE_write_clause(A,B).
call_lambda1(lambda1_72(B,C),A):-private_1_read_clauses_from_stream(A,B,C).
call_lambda1(lambda1_71(F,A,B),D):-private_1_yolo_UNSAFE_read_clause(A,B,C), (C=public_0_some(E)->D=[E|G],call_lambda1(F,G);D=[]).
call_lambda1(lambda1_58,A):-atom(A).
call_lambda1(lambda1_57,A):-var(A).
call_lambda1(lambda1_35(B),A):-member(A,B).
call_lambda1(lambda1_29(A),B):-private_4_typecheckClause(A,B).
call_lambda1(lambda1_18(A),public_0_pair(B,_)):-A==B.
call_lambda1(lambda1_9,public_0_pair(_,A)):-public_0_find(A,lambda1_8,public_0_some(_)).
call_lambda1(lambda1_8,A):-private_5_clpClause(A).
call_lambda1(lambda1_6(B),public_0_pair(A,_)):-private_5_isCallLambda(A),public_0_notMember(A,B).
call_lambda1(lambda1_4(A),B):-public_0_setContains(A,B).
call_lambda2(lambda2_83,public_0_pair(A,_),A).
call_lambda2(lambda2_81(A),public_0_pair(_,B),public_0_pair(_,C)):-call_lambda2(A,B,C).
call_lambda2(lambda2_80(B),A,public_0_pair(A,C)):-call_lambda2(B,A,C).
call_lambda2(lambda2_70,A,B):-private_2_translateClause(A,B).
call_lambda2(lambda2_69(C),A,public_2_typeConstructor(B,E)):-A=..[B|D],private_2_translateTypes(C,D,E).
call_lambda2(lambda2_64(A),B,C):-private_2_translateType(A,B,C).
call_lambda2(lambda2_61,A,B):-private_2_translateTerm(A,B).
call_lambda2(lambda2_56,A/B,public_0_pair(A,B)):-atom(A),number(B).
call_lambda2(lambda2_54(B),public_2_clauseclause(C,A,G),public_2_clauseclause(E,F,H)):-length(A,D),private_3_renamedClause(B,C,D,E),private_3_translateTerms(B,A,F),private_3_translateBody(B,G,H).
call_lambda2(lambda2_53(B),public_2_defglobalvar(C,A,E),public_2_defglobalvar(D,A,F)):-private_3_renamedGlobalVariable(B,C,D),private_3_translateType(B,E,F).
call_lambda2(lambda2_52(C),public_2_defclause(D,A,B),public_2_defclause(F,A,G)):-length(B,E),private_3_renamedClause(C,D,E,F),private_3_translateTypes(C,B,G).
call_lambda2(lambda2_51(A),B,C):-private_3_translateDataDef(A,B,C).
call_lambda2(lambda2_50(A),public_2_typeConstructor(B,D),public_2_typeConstructor(C,E)):-private_3_renamedConstructor(A,B,C),private_3_translateTypes(A,D,E).
call_lambda2(lambda2_49(A),B,C):-private_3_translateType(A,B,C).
call_lambda2(lambda2_48(A),B,C):-private_3_translateTerm(A,B,C).
call_lambda2(lambda2_47(B),public_2_defglobalvar(A,_,_),public_0_pair(A,C)):-private_3_yolo_UNSAFE_mangled_name(private_3_mod_private,B,A,C).
call_lambda2(lambda2_45(C,B),public_2_typeConstructor(A,_),public_0_pair(A,D)):-private_3_yolo_UNSAFE_mangled_name(B,C,A,D).
call_lambda2(lambda2_44(F,D),public_2_defclause(A,_,B),public_0_pair(public_0_pair(A,C),G)):-length(B,C), (member(public_0_pair(A,C),D)->E=private_3_mod_public;E=private_3_mod_private),private_3_yolo_UNSAFE_mangled_name(E,F,A,G).
call_lambda2(lambda2_42(B),A,public_0_pair(A,C)):-private_3_yolo_UNSAFE_mangled_name(private_3_mod_public,B,A,C).
call_lambda2(lambda2_41(B),A,public_0_pair(A,C)):-private_3_yolo_UNSAFE_mangled_name(private_3_mod_public,B,A,C).
call_lambda2(lambda2_40(B),A,public_0_pair(A,D)):-A=public_0_pair(C,_),private_3_yolo_UNSAFE_mangled_name(private_3_mod_public,B,C,D).
call_lambda2(lambda2_36(C),B,A):-A=public_2_defdata(B,_,_),member(A,C).
call_lambda2(lambda2_34(C),private_3_moduleUse(A,_,D),E):-B=private_3_loadedModule(_,A,_,_),member(B,C),private_3_extractConstructors(B,D,E).
call_lambda2(lambda2_33,public_2_defdata(_,_,A),B):-public_0_map(A,lambda2_32,B).
call_lambda2(lambda2_32,public_2_typeConstructor(A,_),A).
call_lambda2(lambda2_17,A,public_0_pair(public_0_pair(B,D),A)):-A=public_2_defclause(B,_,C),length(C,D).
call_lambda2(lambda2_16,A,C):-A=public_2_defdata(_,_,B),public_0_map(B,lambda2_15(A),C).
call_lambda2(lambda2_15(B),public_2_typeConstructor(A,_),public_0_pair(A,B)).
call_lambda2(lambda2_14,public_0_pair(A,_),A).
call_lambda2(lambda2_13,A,B):-A@>B.
call_lambda2(lambda2_12,public_2_clauseclause(A,_,_),A).
call_lambda2(lambda2_10,public_2_clauseclause(A,_,_),B):-private_5_isCallLambda(A)->B=[A];B=[].
call_lambda2(lambda2_7,public_2_clauseclause(A,B,D),public_0_pair(public_0_pair(A,C),E)):-length(B,C),private_5_bodyDirectlyCalls(D,E).
call_lambda2(lambda2_5,A,public_2_term_var(A)).
call_lambda2(lambda2_1,A,B):-private_6_yolo_UNSAFE_translate_clause(A,B).
call_lambda2(lambda2_0,A,B):-private_6_yolo_UNSAFE_translate_term(A,B).
call_lambda3(lambda3_84,public_0_pair(A,C),B,public_0_pair(E,D)):-public_0_setContains(A,B)-> (public_0_setContains(C,B)->D=C;D=[B|C]),E=A;E=[B|A],D=C.
call_lambda3(lambda3_82(B),A,C,D):-private_0_insertItem(A,B,C,D).
call_lambda3(lambda3_79(A),C,B,D):-public_0_setContains(A,B)->C=D;D=[B|C].
call_lambda3(lambda3_78,A,B,C):-public_0_setContains(A,B)->A=C;C=[B|A].
call_lambda3(lambda3_75(A),B,D,E):-call_lambda2(A,B,C),append(C,D,E).
call_lambda3(lambda3_55(A),public_0_tup4(C,E,G,I),B,public_0_tup4(D,F,H,J)):-private_3_translateModule(A,B,C,D,E,F,G,H,I,J).
call_lambda3(lambda3_46(E,C),public_2_defdata(A,_,G),public_0_pair(B,I),public_0_pair([public_0_pair(A,F)|B],J)):- (member(A,C)->D=private_3_mod_public;D=private_3_mod_private),private_3_yolo_UNSAFE_mangled_name(D,E,A,F),public_0_map(G,lambda2_45(E,D),H),append(H,I,J).
call_lambda3(lambda3_43(K),private_3_moduleUse(B,A,F),public_0_tup3(D,H,N),public_0_tup3(E,I,O)):-public_0_map(A,lambda2_40(B),C),append(C,D,E),public_0_map(F,lambda2_41(B),G),append(G,H,I),J=private_3_loadedModule(_,B,_,_),member(J,K),private_3_extractConstructors(J,F,L),public_0_map(L,lambda2_42(B),M),append(M,N,O).
call_lambda3(lambda3_37(H,E),public_0_pair(G,[private_3_moduleUse(J,A,B)|C]),public_2_def_use_module(D,A,B),public_0_pair(I,C)):-private_3_yolo_UNSAFE_absolute_file_name(D,E,F),private_3_loadModule(F,G,H,I),member(private_3_loadedModule(F,J,_,_),I).
call_lambda3(lambda3_28,A,public_2_typevar(A),B):-B is A.
call_lambda3(lambda3_27(A),B,public_0_pair(C,D),E):-private_4_typeofTerm(A,B,C,D,E).
call_lambda3(lambda3_11,public_0_pair([C|A],D),B,public_0_pair(A,E)):-private_5_translateClause(B,C,D,E).
call_lambda6(lambda6_3,A,B,C,D,E,F):-private_5_translateBody(A,B,C,D,E,F).
call_lambda6(lambda6_2,A,B,C,D,E,F):-private_5_translateTerm(A,B,C,D,E,F).
private_7_useClp:-A#=A.
private_7_processFile(A,F,H):-public_3_handleModules(A,B,C,D,E),!,public_4_typecheckClauses(B,C,D,E),!,public_5_translateClauses(E,F,G),!,public_6_writeTranslatedClauses(G,H),!.
public_7_processFileForSwipl(A,B):-private_7_processFile(A,public_5_swipl,B).
public_7_processFileForGnuProlog(A,B):-private_7_processFile(A,public_5_gnuprolog,B).
private_6_translateOp(public_2_plus,+).
private_6_translateOp(public_2_minus,-).
private_6_translateOp(public_2_mul,*).
private_6_translateOp(public_2_div,/).
private_6_translateOp(public_2_op_min,min).
private_6_translateOp(public_2_op_max,max).
private_6_translateBodyUnaryOp(public_2_not,\+).
private_6_translateBodyPairOp(public_2_and,',').
private_6_translateBodyPairOp(public_2_or,;).
private_6_translateBodyPairOp(public_2_implies,->).
private_6_yolo_UNSAFE_translate_exp(public_2_exp_var(A),B):-A=B.
private_6_yolo_UNSAFE_translate_exp(public_2_exp_num(A),B):-A=B.
private_6_yolo_UNSAFE_translate_exp(public_2_binop(B,A,C),D):-private_6_translateOp(A,E),private_6_yolo_UNSAFE_translate_exp(B,F),private_6_yolo_UNSAFE_translate_exp(C,G),D=..[E,F,G].
private_6_yolo_UNSAFE_translate_exp_lhs(public_2_lhs_var(A),B):-A=B.
private_6_yolo_UNSAFE_translate_exp_lhs(public_2_lhs_num(A),B):-A=B.
private_6_translateTerms(A,B):-public_0_map(A,lambda2_0,B).
private_6_yolo_UNSAFE_translate_term(public_2_term_var(A),B):-A=B.
private_6_yolo_UNSAFE_translate_term(public_2_term_num(A),B):-A=B.
private_6_yolo_UNSAFE_translate_term(public_2_term_constructor(C,A),B):-private_6_translateTerms(A,D),B=..[C|D].
private_6_yolo_UNSAFE_translate_body(public_2_body_is(A,B),C):-private_6_yolo_UNSAFE_translate_exp_lhs(A,D),private_6_yolo_UNSAFE_translate_exp(B,E),C=..[is,D,E].
private_6_yolo_UNSAFE_translate_body(public_2_bodyUnary(A,B),C):-private_6_translateBodyUnaryOp(A,D),private_6_yolo_UNSAFE_translate_body(B,E),C=..[D,E].
private_6_yolo_UNSAFE_translate_body(public_2_bodyPair(B,A,C),D):-private_6_translateBodyPairOp(A,E),private_6_yolo_UNSAFE_translate_body(B,F),private_6_yolo_UNSAFE_translate_body(C,G),D=..[E,F,G].
private_6_yolo_UNSAFE_translate_body(public_2_firstOrderCall(C,A),B):-private_6_translateTerms(A,D),B=..[C|D].
private_6_yolo_UNSAFE_translate_clause(public_2_clauseclause(C,A,B),F):-private_6_translateTerms(A,D),private_6_yolo_UNSAFE_translate_body(B,E),G=..[C|D], (E==true->F=G;F=..[:-,G,E]).
public_6_writeTranslatedClauses(A,C):-public_0_map(A,lambda2_1,B),public_1_writeClauses(B,C).
private_5_freshInt(A):-nb_getval(private_5_counter,A),B is A+1,nb_setval(private_5_counter,B).
private_5_yolo_UNSAFE_call_lambda_label(B,A):-format(atom(A),'call_lambda~d',[B]).
private_5_yolo_UNSAFE_fresh_lambda_label(B,A):-private_5_freshInt(C),format(atom(A),'lambda~d_~d',[B,C]).
private_5_engineSetVarName(public_5_swipl,nb_setval).
private_5_engineSetVarName(public_5_gnuprolog,g_assign).
private_5_engineGetVarName(public_5_swipl,nb_getval).
private_5_engineGetVarName(public_5_gnuprolog,g_read).
private_5_yolo_UNSAFE_term_variables(A,B):-term_variables(A,B).
private_5_translateMulti(_,[],[],[],A,A,_).
private_5_translateMulti(B,[C|H],M,[D|I],E,K,A):-call_lambda6(A,B,C,F,D,E,J),public_0_setUnion(F,B,G),private_5_translateMulti(G,H,L,I,J,K,A),public_0_setUnion(F,L,M).
private_5_translateTerms(A,B,C,D,E,F):-private_5_translateMulti(A,B,C,D,E,F,lambda6_2).
private_5_translateBodies(A,B,C,D,E,F):-private_5_translateMulti(A,B,C,D,E,F,lambda6_3).
private_5_translateBody(_,A,C,A,B,B):-A=public_2_body_is(_,_),!,private_5_yolo_UNSAFE_term_variables(A,C).
private_5_translateBody(D,public_2_body_setvar(A,E),F,public_2_firstOrderCall(C,[public_2_term_constructor(A,[]),G]),H,I):-!,nb_getval(private_5_engine,B),private_5_engineSetVarName(B,C),private_5_translateTerm(D,E,F,G,H,I).
private_5_translateBody(D,public_2_body_getvar(A,E),F,public_2_firstOrderCall(C,[public_2_term_constructor(A,[]),G]),H,I):-!,nb_getval(private_5_engine,B),private_5_engineGetVarName(B,C),private_5_translateTerm(D,E,F,G,H,I).
private_5_translateBody(B,public_2_bodyUnary(A,C),D,public_2_bodyUnary(A,E),F,G):-!,private_5_translateBody(B,C,D,E,F,G).
private_5_translateBody(B,public_2_bodyPair(C,A,D),E,public_2_bodyPair(F,A,G),H,I):-!,private_5_translateBodies(B,[C,D],E,[F,G],H,I).
private_5_translateBody(A,public_2_higherOrderCall(B,C),D,public_2_firstOrderCall(I,E),F,G):-!,private_5_translateTerms(A,[B|C],D,E,F,G),length(C,H),private_5_yolo_UNSAFE_call_lambda_label(H,I).
private_5_translateBody(A,public_2_firstOrderCall(G,B),C,I,D,E):-!,nb_getval(private_5_engine,F),private_5_translateTerms(A,B,C,H,D,E),private_5_translateCall(F,G,H,I).
private_5_translateCall(public_5_swipl,fd_labeling,A,public_2_firstOrderCall(label,A)):-!.
private_5_translateCall(_,A,B,public_2_firstOrderCall(A,B)).
private_5_translateTerm(_,public_2_term_var(A),[A],public_2_term_var(A),B,B):-!.
private_5_translateTerm(_,public_2_term_num(A),[],public_2_term_num(A),B,B):-!.
private_5_translateTerm(B,public_2_term_lambda(C,G),P,A,D,W):-A=public_2_term_constructor(J,Q),!,private_5_translateTerms(B,C,E,S,D,H),public_0_setUnion(B,E,F),private_5_translateBody(F,G,K,T,H,U),length(C,I),private_5_yolo_UNSAFE_fresh_lambda_label(I,J),public_0_setDifference(E,B,M),append(E,K,L),public_0_setDifference(L,M,N),public_0_filter(N,lambda1_4(B),O),public_0_makeSetFromList(O,P),public_0_map(P,lambda2_5,Q),private_5_yolo_UNSAFE_call_lambda_label(I,R),V=public_2_clauseclause(R,[A|S],T),U=[V|W].
private_5_translateTerm(B,public_2_term_constructor(A,C),D,public_2_term_constructor(A,E),F,G):-!,private_5_translateTerms(B,C,D,E,F,G).
private_5_translateClause(public_2_clauseclause(A,B,F),public_2_clauseclause(A,C,G),D,I):-private_5_translateTerms([],B,E,C,D,H),private_5_translateBody(E,F,_,G,H,I).
private_5_bodyDirectlyCalls(public_2_body_is(_,_),A,A).
private_5_bodyDirectlyCalls(public_2_body_setvar(_,_),A,A).
private_5_bodyDirectlyCalls(public_2_body_getvar(_,_),A,A).
private_5_bodyDirectlyCalls(public_2_bodyUnary(_,A),B,C):-private_5_bodyDirectlyCalls(A,B,C).
private_5_bodyDirectlyCalls(public_2_bodyPair(A,_,C),B,E):-private_5_bodyDirectlyCalls(A,B,D),private_5_bodyDirectlyCalls(C,D,E).
private_5_bodyDirectlyCalls(public_2_firstOrderCall(A,C),[public_0_pair(A,D)|B],B):-length(C,D).
private_5_bodyDirectlyCalls(A,B):-private_5_bodyDirectlyCalls(A,B,[]).
private_5_clauseNameArity(public_2_clauseclause(A,B,_),A,C):-length(B,C).
private_5_callsUnknownLambda(A,B):-public_0_find(A,lambda1_6(B),public_0_some(_)),!.
private_5_trimDeadClauses([],_,_,_,A,B):-reverse(A,B).
private_5_trimDeadClauses([A|K],E,H,G,I,M):-private_5_clauseNameArity(A,B,C),D=public_0_pair(B,C),member(public_0_pair(D,F),E), ((public_0_setsOverlap(F,G);private_5_callsUnknownLambda(F,H))->reverse(I,J),append(J,K,L),private_5_trimDeadClauses(L,E,H,[D|G],[],M);private_5_trimDeadClauses(K,E,H,G,[A|I],M)).
private_5_isCallLambda(A):-atom_codes(call_lambda,C),atom_codes(A,B),public_0_beginsWith(B,C).
private_5_clauseCallsMapping(A,B):-public_0_map(A,lambda2_7,B).
private_5_clpClauseName(#>).
private_5_clpClauseName(#<).
private_5_clpClauseName(#=<).
private_5_clpClauseName(#>=).
private_5_clpClauseName(#=).
private_5_clpClauseName(#\=).
private_5_clpClause(public_0_pair(A,2)):-private_5_clpClauseName(A).
private_5_clpUsed(A):-public_0_find(A,lambda1_9,public_0_some(_)),!.
private_5_makeDirective(A,public_2_clauseclause(:-,[A],public_2_firstOrderCall(true,[]))).
private_5_handleClp(A,public_5_swipl,D,B):-private_5_clpUsed(A)->private_5_makeDirective(public_2_term_constructor(use_module,[public_2_term_constructor(library,[public_2_term_constructor(clpfd,[])])]),C),B=[C|D];D=B.
private_5_handleClp(_,public_5_gnuprolog,A,A).
private_5_trimDeadClauses(C,A,E):-public_0_flatMap(A,lambda2_10,B),public_0_makeSetFromList(B,D),private_5_trimDeadClauses(A,C,D,[],[],E).
public_5_translateClauses(B,A,I):-nb_setval(private_5_counter,0),nb_setval(private_5_engine,A),public_0_foldLeft(B,public_0_pair(E,C),lambda3_11,public_0_pair([],[])),public_0_sortItems(C,lambda2_12,lambda2_13,D),append(D,E,F),private_5_clauseCallsMapping(F,G),private_5_trimDeadClauses(G,F,H),private_5_handleClp(G,A,H,I).
private_4_builtinDataDefs([public_2_defdata(list,[A],[public_2_typeConstructor('.',[A,public_2_constructorType(list,[A])]),public_2_typeConstructor([],[])])]).
private_4_builtinClauseDefs([public_2_defclause(true,[],[]),public_2_defclause(false,[],[]),public_2_defclause(fail,[],[]),public_2_defclause(!,[],[]),public_2_defclause(var,[A],[A]),public_2_defclause(nonvar,[A],[A]),public_2_defclause(atom,[A],[A]),public_2_defclause(>,[],[public_2_intType,public_2_intType]),public_2_defclause(<,[],[public_2_intType,public_2_intType]),public_2_defclause(=<,[],[public_2_intType,public_2_intType]),public_2_defclause(>=,[],[public_2_intType,public_2_intType]),public_2_defclause(@>,[A],[A,A]),public_2_defclause(@<,[A],[A,A]),public_2_defclause(@=<,[A],[A,A]),public_2_defclause(@>=,[A],[A,A]),public_2_defclause(=,[A],[A,A]),public_2_defclause(\=,[A],[A,A]),public_2_defclause(#>,[],[public_2_intType,public_2_intType]),public_2_defclause(#<,[],[public_2_intType,public_2_intType]),public_2_defclause(#=<,[],[public_2_intType,public_2_intType]),public_2_defclause(#>=,[],[public_2_intType,public_2_intType]),public_2_defclause(#=,[],[public_2_intType,public_2_intType]),public_2_defclause(#\=,[],[public_2_intType,public_2_intType]),public_2_defclause(fd_labeling,[],[public_2_constructorType(list,[public_2_intType])]),public_2_defclause(==,[A],[A,A]),public_2_defclause(\==,[A],[A,A]),public_2_defclause(is_set,[A],[public_2_constructorType(list,[A])]),public_2_defclause(member,[A],[A,public_2_constructorType(list,[A])]),public_2_defclause(reverse,[A],[public_2_constructorType(list,[A]),public_2_constructorType(list,[A])]),public_2_defclause(copy_term,[A],[A,A]),public_2_defclause(append,[A],[public_2_constructorType(list,[A]),public_2_constructorType(list,[A]),public_2_constructorType(list,[A])]),public_2_defclause(length,[A],[public_2_constructorType(list,[A]),public_2_intType]),public_2_defclause(atom_codes,[],[public_2_atomType,public_2_constructorType(list,[public_2_intType])])]).
private_4_keys(A,B):-public_0_map(A,lambda2_14,B).
private_4_mappingUnique(A):-private_4_keys(A,B),is_set(B).
private_4_constructorToDataDefMapping(A,B):-public_0_flatMap(A,lambda2_16,B),private_4_mappingUnique(B).
private_4_clauseToClauseDefMapping(A,B):-public_0_map(A,lambda2_17,B),private_4_mappingUnique(B).
private_4_makeState(B,D,A,private_4_state(C,E,A)):-private_4_constructorToDataDefMapping(B,C),private_4_clauseToClauseDefMapping(D,E).
private_4_expectedFormalParamTypes(private_4_state(_,C,_),A,B,E,F):-member(public_0_pair(public_0_pair(A,B),D),C),copy_term(D,public_2_defclause(_,E,F)).
private_4_expectedFormalParamTypes(A,B,C,D):-private_4_expectedFormalParamTypes(A,B,C,_,D).
private_4_envVariableType(A,B,D,F):-public_0_find(A,lambda1_18(B),C),!, (C=public_0_some(public_0_pair(_,E))->D=E,A=F;F=[public_0_pair(B,D)|A]).
private_4_typecheckLhs(A,public_2_lhs_num(_),A):-!.
private_4_typecheckLhs(A,public_2_lhs_var(B),C):-!,private_4_envVariableType(A,B,public_2_intType,C).
private_4_typecheckExp(A,public_2_exp_var(B),C):-!,private_4_envVariableType(A,B,public_2_intType,C).
private_4_typecheckExp(A,public_2_exp_num(_),A):-!.
private_4_typecheckExp(A,public_2_binop(B,_,D),E):-!,private_4_typecheckExp(A,B,C),!,private_4_typecheckExp(C,D,E).
private_4_typecheckVarUse(A,D,B,E,G):-A=private_4_state(_,_,C),member(public_2_defglobalvar(B,_,F),C),private_4_typeofTerm(A,D,E,F,G).
private_4_typecheckBody(D,A,C,B):-public_0_onFailure(lambda0_19(A,B,C,D),lambda0_20(C)).
private_4_rawTypecheckBody(_,A,public_2_body_is(B,D),E):-!,private_4_typecheckLhs(A,B,C),!,private_4_typecheckExp(C,D,E),!.
private_4_rawTypecheckBody(A,B,public_2_body_setvar(C,D),E):-!,private_4_typecheckVarUse(A,B,C,D,E),!.
private_4_rawTypecheckBody(A,B,public_2_body_getvar(C,D),E):-!,private_4_typecheckVarUse(A,B,C,D,E),!.
private_4_rawTypecheckBody(A,B,public_2_bodyUnary(_,C),D):-!,private_4_typecheckBody(A,B,C,D),!.
private_4_rawTypecheckBody(A,B,public_2_bodyPair(C,_,E),F):-!,private_4_typecheckBody(A,B,C,D),!,private_4_typecheckBody(A,D,E,F),!.
private_4_rawTypecheckBody(A,B,public_2_higherOrderCall(C,E),G):-!,private_4_typeofTerm(A,B,C,public_2_relationType(F),D),!,private_4_typeofTerms(A,D,E,F,G),!.
private_4_rawTypecheckBody(E,F,public_2_firstOrderCall(B,A),G):-length(A,D),C=_,public_0_onFailure(lambda0_21(B,C,D,E),lambda0_22(B,D)),private_4_typeofTerms(E,F,A,C,G),!.
private_4_typeofTerm(E,A,D,B,C):-public_0_onFailure(lambda0_23(A,B,C,D,E),lambda0_26(A,E,B,D)).
private_4_rawTypeofTerm(_,A,public_2_term_var(B),C,D):-private_4_envVariableType(A,B,C,D).
private_4_rawTypeofTerm(_,A,public_2_term_num(_),public_2_intType,A).
private_4_rawTypeofTerm(B,A,public_2_term_lambda(C,F),public_2_relationType(D),A):-!,private_4_typeofTerms(B,A,C,D,E),private_4_typecheckBody(B,E,F,_).
private_4_rawTypeofTerm(A,H,public_2_term_constructor(B,I),public_2_constructorType(E,F),K):-A=private_4_state(C,_,_),member(public_0_pair(B,D),C),copy_term(D,public_2_defdata(E,F,G)),member(public_2_typeConstructor(B,J),G),!,private_4_typeofTerms(A,H,I,J,K),!.
private_4_rawTypeofTerm(_,A,public_2_term_constructor(_,[]),public_2_atomType,A).
private_4_typeofTerms(E,D,A,B,F):-public_0_zip(A,B,C),public_0_foldLeft(C,D,lambda3_27(E),F),!.
private_4_markedUnsafe(A):-public_0_atomContains(A,yolo_UNSAFE_).
private_4_typecheckClause(B,public_2_clauseclause(C,A,H)):-length(A,D),private_4_expectedFormalParamTypes(B,C,D,E,F),public_0_foldLeft(E,0,lambda3_28,_),private_4_typeofTerms(B,[],A,F,G), (private_4_markedUnsafe(C)->true;private_4_typecheckBody(B,G,H,_)),!.
public_4_typecheckClauses(B,D,G,H):-private_4_builtinDataDefs(A),private_4_builtinClauseDefs(C),append(A,B,E),append(C,D,F),private_4_makeState(E,F,G,I),public_0_forall(H,lambda1_29(I)).
private_3_yolo_UNSAFE_mangled_name(A,D,E,C):- (A==private_3_mod_public->B= (public);B=private),format(atom(C),'~a_~d_~a',[B,D,E]).
private_3_freshModuleId(A):-nb_getval(private_3_counter,A),B is A+1,nb_setval(private_3_counter,B).
private_3_yolo_UNSAFE_absolute_file_name(A,C,B):-absolute_file_name(A,B,[relative_to(C)]),public_0_onFailure(lambda0_30(B),lambda0_31(B)).
private_3_constructorsInDataDefs(A,B):-public_0_flatMap(A,lambda2_33,B).
private_3_allImportedConstructors(B,A,C):-public_0_flatMap(A,lambda2_34(B),C).
private_3_extractConstructors(private_3_loadedModule(_,_,_,A),B,F):-A=public_2_loadedFile(public_2_defmodule(_,_,C),_,D,_,_,_),public_0_forall(B,lambda1_35(C)),public_0_map(B,lambda2_36(D),E),private_3_constructorsInDataDefs(E,F).
private_3_directLoadModule(A,D,F,[private_3_loadedModule(A,L,E,B)|G]):-public_2_loadFile(A,B),B=public_2_loadedFile(_,C,H,_,_,_),public_0_foldLeft(C,public_0_pair(D,E),lambda3_37(F,A),public_0_pair(G,[])),private_3_allImportedConstructors(G,E,I),private_3_constructorsInDataDefs(H,J),append(I,J,K),public_0_onFailure(lambda0_38(K),lambda0_39(K)),private_3_freshModuleId(L).
private_3_renamedClause(private_3_renaming(D,_,_,_),A,B,C):-member(public_0_pair(public_0_pair(A,B),C),D),!.
private_3_renamedClause(_,A,_,A).
private_3_renamedType(private_3_renaming(_,C,_,_),A,B):-member(public_0_pair(A,B),C),!.
private_3_renamedType(_,A,A).
private_3_renamedConstructor(private_3_renaming(_,_,C,_),A,B):-member(public_0_pair(A,B),C),!.
private_3_renamedConstructor(_,A,A).
private_3_renamedGlobalVariable(private_3_renaming(_,_,_,C),A,B):-member(public_0_pair(A,B),C),!.
private_3_renamedGlobalVariable(_,A,A).
private_3_makeRenaming(C,private_3_loadedModule(_,E,B,A),private_3_renaming(M,P,S,J)):-A=public_2_loadedFile(public_2_defmodule(_,F,H),_,G,D,I,_),public_0_foldRight(B,public_0_tup3([],[],[]),lambda3_43(C),public_0_tup3(L,O,R)),public_0_map(D,lambda2_44(E,F),K),public_0_foldRight(G,public_0_pair([],[]),lambda3_46(E,H),public_0_pair(N,Q)),public_0_map(I,lambda2_47(E),J),append(K,L,M),append(N,O,P),append(Q,R,S).
private_3_loadModule(A,C,B,D):-public_0_notMember(A,B), (member(private_3_loadedModule(A,_,_,_),C)->D=C;private_3_directLoadModule(A,C,[A|B],D)).
private_3_translateVarUse(A,B,D,C,E):-private_3_renamedGlobalVariable(A,B,C),private_3_translateTerm(A,D,E).
private_3_translateBody(_,public_2_body_is(A,B),public_2_body_is(A,B)).
private_3_translateBody(A,public_2_body_setvar(B,C),public_2_body_setvar(D,E)):-private_3_translateVarUse(A,B,C,D,E).
private_3_translateBody(A,public_2_body_getvar(B,C),public_2_body_getvar(D,E)):-private_3_translateVarUse(A,B,C,D,E).
private_3_translateBody(B,public_2_bodyUnary(A,C),public_2_bodyUnary(A,D)):-private_3_translateBody(B,C,D).
private_3_translateBody(B,public_2_bodyPair(C,A,E),public_2_bodyPair(D,A,F)):-private_3_translateBody(B,C,D),private_3_translateBody(B,E,F).
private_3_translateBody(A,public_2_higherOrderCall(B,D),public_2_higherOrderCall(C,E)):-private_3_translateTerm(A,B,C),private_3_translateTerms(A,D,E).
private_3_translateBody(B,public_2_firstOrderCall(C,A),public_2_firstOrderCall(E,F)):-length(A,D),private_3_renamedClause(B,C,D,E),private_3_translateTerms(B,A,F).
private_3_translateTerms(B,A,C):-public_0_map(A,lambda2_48(B),C).
private_3_translateTerm(_,public_2_term_var(A),public_2_term_var(A)).
private_3_translateTerm(_,public_2_term_num(A),public_2_term_num(A)).
private_3_translateTerm(A,public_2_term_lambda(B,D),public_2_term_lambda(C,E)):-private_3_translateTerms(A,B,C),private_3_translateBody(A,D,E).
private_3_translateTerm(A,public_2_term_constructor(B,D),public_2_term_constructor(C,E)):-private_3_renamedConstructor(A,B,C),private_3_translateTerms(A,D,E).
private_3_translateTypes(B,A,C):-public_0_map(A,lambda2_49(B),C).
private_3_translateType(_,A,B):-var(A),!,A=B.
private_3_translateType(_,public_2_intType,public_2_intType):-!.
private_3_translateType(_,public_2_atomType,public_2_atomType):-!.
private_3_translateType(A,public_2_relationType(B),public_2_relationType(C)):-!,private_3_translateTypes(A,B,C).
private_3_translateType(A,public_2_constructorType(B,D),public_2_constructorType(C,E)):-!,private_3_renamedType(A,B,C),private_3_translateTypes(A,D,E).
private_3_translateDataDef(B,public_2_defdata(C,A,E),public_2_defdata(D,A,F)):-private_3_renamedType(B,C,D),public_0_map(E,lambda2_50(B),F).
private_3_translateLoadedFile(B,public_2_loadedFile(_,_,A,F,J,N),D,E,H,I,L,M,P,Q):-public_0_map(A,lambda2_51(B),C),public_0_appendDiffList(C,D,E),public_0_map(F,lambda2_52(B),G),public_0_appendDiffList(G,H,I),public_0_map(J,lambda2_53(B),K),public_0_appendDiffList(K,L,M),public_0_map(N,lambda2_54(B),O),public_0_appendDiffList(O,P,Q).
private_3_translateModule(B,A,E,F,G,H,I,J,K,L):-A=private_3_loadedModule(_,_,_,D),private_3_makeRenaming(B,A,C),private_3_translateLoadedFile(C,D,E,F,G,H,I,J,K,L),!.
public_3_handleModules(A,D,E,F,G):-nb_setval(private_3_counter,0),private_3_yolo_UNSAFE_absolute_file_name(A,./,B),private_3_directLoadModule(B,[],[B],C),!,public_0_foldLeft(C,public_0_tup4(D,E,F,G),lambda3_55(C),public_0_tup4([],[],[],[])).
private_2_yolo_UNSAFE_translate_pairs(A,B):-public_0_map(A,lambda2_56,B).
private_2_areTypeVars(A):-public_0_forall(A,lambda1_57),is_set(A).
private_2_allAtoms(A):-public_0_forall(A,lambda1_58).
private_2_yolo_UNSAFE_translate_exp_lhs(A,public_2_lhs_var(B)):-var(A),!,A=B.
private_2_yolo_UNSAFE_translate_exp_lhs(A,public_2_lhs_num(B)):-number(A),!,A=B.
private_2_yolo_UNSAFE_translate_op(A,public_2_plus):-A= (+),!.
private_2_yolo_UNSAFE_translate_op(A,public_2_minus):-A= (-),!.
private_2_yolo_UNSAFE_translate_op(A,public_2_mul):-A= (*),!.
private_2_yolo_UNSAFE_translate_op(A,public_2_div):-A= (/),!.
private_2_yolo_UNSAFE_translate_op(A,public_2_op_min):-A=min,!.
private_2_yolo_UNSAFE_translate_op(A,public_2_op_max):-A=max,!.
private_2_yolo_UNSAFE_translate_exp(A,public_2_exp_var(B)):-var(A),!,A=B.
private_2_yolo_UNSAFE_translate_exp(A,public_2_exp_num(B)):-number(A),!,A=B.
private_2_yolo_UNSAFE_translate_exp(A,public_2_binop(E,C,G)):-A=..[B,D,F],!,private_2_yolo_UNSAFE_translate_op(B,C),private_2_yolo_UNSAFE_translate_exp(D,E),private_2_yolo_UNSAFE_translate_exp(F,G).
private_2_yolo_UNSAFE_translate_body_pair_op(A,public_2_and):-A= (','),!.
private_2_yolo_UNSAFE_translate_body_pair_op(A,public_2_or):-A= (;),!.
private_2_yolo_UNSAFE_translate_body_pair_op(A,public_2_implies):-A= (->),!.
private_2_yolo_UNSAFE_translate_unary_body_op(A,public_2_not):-A= (\+),!.
private_2_translateBody(B,A):-public_0_onFailure(lambda0_59(A,B),lambda0_60(B)).
private_2_yolo_UNSAFE_translate_body(A,public_2_body_is(C,E)):-A= (B is D),!,private_2_yolo_UNSAFE_translate_exp_lhs(B,C),private_2_yolo_UNSAFE_translate_exp(D,E).
private_2_yolo_UNSAFE_translate_body(A,public_2_body_setvar(B,D)):-A=setvar(B,C),!,atom(B),private_2_translateTerm(C,D).
private_2_yolo_UNSAFE_translate_body(A,public_2_body_getvar(B,D)):-A=getvar(B,C),!,atom(B),private_2_translateTerm(C,D).
private_2_yolo_UNSAFE_translate_body(A,public_2_bodyUnary(C,E)):-A=..[B,D],private_2_yolo_UNSAFE_translate_unary_body_op(B,C),!,private_2_translateBody(D,E).
private_2_yolo_UNSAFE_translate_body(A,public_2_bodyPair(E,C,G)):-A=..[B,D,F],private_2_yolo_UNSAFE_translate_body_pair_op(B,C),!,private_2_translateBody(D,E),private_2_translateBody(F,G).
private_2_yolo_UNSAFE_translate_body(A,public_2_higherOrderCall(C,E)):-A=..[call,B|D],!,private_2_translateTerm(B,C),private_2_translateTerms(D,E).
private_2_yolo_UNSAFE_translate_body(A,public_2_firstOrderCall(B,D)):-A=..[B|C],!,private_2_translateTerms(C,D).
private_2_translateTerms(A,B):-public_0_map(A,lambda2_61,B).
private_2_translateTerm(B,A):-public_0_onFailure(lambda0_62(A,B),lambda0_63(B)).
private_2_yolo_UNSAFE_translate_term(A,public_2_term_var(B)):-var(A),!,A=B.
private_2_yolo_UNSAFE_translate_term(A,public_2_term_num(B)):-number(A),!,A=B.
private_2_yolo_UNSAFE_translate_term(A,public_2_term_lambda(C,E)):-A=..[lambda,B,D],!,private_2_translateTerms(B,C),private_2_translateBody(D,E).
private_2_yolo_UNSAFE_translate_term(A,public_2_term_constructor(B,D)):-A=..[B|C],private_2_translateTerms(C,D).
private_2_yolo_UNSAFE_normalize_clause(A,A):-A= (_:-_),!.
private_2_yolo_UNSAFE_normalize_clause(B,A):-A= (B:-true).
private_2_translateTypes(B,A,C):-public_0_map(A,lambda2_64(B),C).
private_2_yolo_UNSAFE_translate_type(B,A,C):-var(A),!,public_0_setContains(B,A),A=C.
private_2_yolo_UNSAFE_translate_type(_,A,public_2_intType):-A=int,!.
private_2_yolo_UNSAFE_translate_type(_,A,public_2_atomType):-A=atom,!.
private_2_yolo_UNSAFE_translate_type(B,A,public_2_relationType(D)):-A=relation(C),!,private_2_translateTypes(B,C,D).
private_2_yolo_UNSAFE_translate_type(C,A,public_2_constructorType(B,E)):-A=..[B|D],!,private_2_translateTypes(C,D,E).
private_2_translateType(C,A,B):-public_0_onFailure(lambda0_65(A,B,C),lambda0_66(A)).
private_2_translateClause(B,A):-public_0_onFailure(lambda0_67(A,B),lambda0_68(B)).
private_2_yolo_UNSAFE_translate_clause(A,private_2_readDefModule(public_2_defmodule(B,E,C))):-A=module(B,D,C),!,atom(B),private_2_yolo_UNSAFE_translate_pairs(D,E),private_2_allAtoms(C).
private_2_yolo_UNSAFE_translate_clause(A,private_2_readDefUseModule(public_2_def_use_module(B,E,C))):-A=use_module(B,D,C),!,atom(B),private_2_yolo_UNSAFE_translate_pairs(D,E),private_2_allAtoms(C).
private_2_yolo_UNSAFE_translate_clause(A,private_2_readDefData(public_2_defdata(B,C,E))):-A=datadef(B,C,D),!,atom(B),private_2_areTypeVars(C),public_0_map(D,lambda2_69(C),E).
private_2_yolo_UNSAFE_translate_clause(A,private_2_readDefClause(public_2_defclause(B,C,E))):-A=clausedef(B,C,D),!,atom(B),private_2_areTypeVars(C),private_2_translateTypes(C,D,E).
private_2_yolo_UNSAFE_translate_clause(A,private_2_readDefGlobalVar(public_2_defglobalvar(B,C,E))):-A=globalvardef(B,C,D),!,atom(B),private_2_areTypeVars(C),private_2_translateType(C,D,E).
private_2_yolo_UNSAFE_translate_clause(A,private_2_readClauseClause(public_2_clauseclause(C,E,G))):-private_2_yolo_UNSAFE_normalize_clause(A, (B:-F)),B=..[C|D],private_2_translateTerms(D,E),private_2_translateBody(F,G).
private_2_sortClause(private_2_readDefModule(A),[A|B],B,C,C,D,D,E,E,F,F,G,G).
private_2_sortClause(private_2_readDefUseModule(B),A,A,[B|C],C,D,D,E,E,F,F,G,G).
private_2_sortClause(private_2_readDefData(C),A,A,B,B,[C|D],D,E,E,F,F,G,G).
private_2_sortClause(private_2_readDefClause(D),A,A,B,B,C,C,[D|E],E,F,F,G,G).
private_2_sortClause(private_2_readDefGlobalVar(E),A,A,B,B,C,C,D,D,[E|F],F,G,G).
private_2_sortClause(private_2_readClauseClause(F),A,A,B,B,C,C,D,D,E,E,[F|G],G).
private_2_sortClauses([],A,A,B,B,C,C,D,D,E,E,F,F).
private_2_sortClauses([A|H],B,J,C,L,D,N,E,P,F,R,G,T):-private_2_sortClause(A,B,I,C,K,D,M,E,O,F,Q,G,S),private_2_sortClauses(H,I,J,K,L,M,N,O,P,Q,R,S,T).
public_2_loadFile(A,public_2_loadedFile(C,D,E,F,G,H)):-public_1_read_clauses_from_file(A,lambda2_70,B),private_2_sortClauses(B,[C],[],D,[],E,[],F,[],G,[],H,[]).
private_1_yolo_UNSAFE_translate_mode(private_1_read_mode,A):-A=read,!.
private_1_yolo_UNSAFE_translate_mode(private_1_write_mode,A):-A=write,!.
private_1_yolo_UNSAFE_open_file(B,A,private_1_stream(D)):-private_1_yolo_UNSAFE_translate_mode(A,C),open(B,C,D).
private_1_yolo_UNSAFE_close_file(private_1_stream(A)):-close(A).
private_1_withOpenStream(A,B,C):-private_1_yolo_UNSAFE_open_file(A,B,D), (call_lambda1(C,D),private_1_yolo_UNSAFE_close_file(D),!;private_1_yolo_UNSAFE_close_file(D),fail).
private_1_yolo_UNSAFE_read_clause(private_1_stream(A),D,C):-read_clause(A,B,[]), (B==end_of_file->C=public_0_none;call_lambda2(D,B,E),C=public_0_some(E)).
private_1_read_clauses_from_stream(B,C,D):-A=lambda1_71(A,B,C),call_lambda1(A,D).
public_1_read_clauses_from_file(A,B,C):-private_1_withOpenStream(A,private_1_read_mode,lambda1_72(B,C)).
private_1_yolo_UNSAFE_write_clause(A,private_1_stream(C)):-copy_term(A,B),numbervars(B,0,_,[singletons(true)]),write_term(C,B,[numbervars(true),quoted(true)]),format(C,'.~n',[]).
public_1_writeClauses(B,A):-private_1_withOpenStream(A,private_1_write_mode,lambda1_74(B)).
public_0_map([],_,[]).
public_0_map([B|D],A,[C|E]):-call_lambda2(A,B,C),public_0_map(D,A,E).
public_0_flatMap(A,B,C):-public_0_foldRight(A,[],lambda3_75(B),C).
public_0_filter([],_,[]).
public_0_filter([B|E],A,C):- (call_lambda1(A,B)->C=[B|D];C=D),public_0_filter(E,A,D).
public_0_foldRight([],A,_,A).
public_0_foldRight([D|A],B,C,F):-public_0_foldRight(A,B,C,E),call_lambda3(C,D,E,F).
public_0_foldLeft([],A,_,A).
public_0_foldLeft([C|D],B,A,F):-call_lambda3(A,B,C,E),public_0_foldLeft(D,E,A,F).
public_0_forall([],_).
public_0_forall([B|C],A):-call_lambda1(A,B),public_0_forall(C,A).
public_0_zip([],[],[]).
public_0_zip([A|C],[B|D],[public_0_pair(A,B)|E]):-public_0_zip(C,D,E).
public_0_setContains([A|_],B):-A==B.
public_0_setContains([_|A],B):-public_0_setContains(A,B).
public_0_setsOverlap(A,B):-public_0_find(A,lambda1_76(B),public_0_some(_)),!.
public_0_find([],_,public_0_none).
public_0_find([A|_],B,public_0_some(A)):-call_lambda1(B,A).
public_0_find([_|A],B,C):-public_0_find(A,B,C).
public_0_beginsWith(_,[]).
public_0_beginsWith([A|B],[A|C]):-public_0_beginsWith(B,C).
public_0_contains(A,B):-public_0_beginsWith(A,B).
public_0_contains([_|A],B):-public_0_contains(A,B).
public_0_notMember(B,A):-public_0_forall(A,lambda1_77(B)).
public_0_atomContains(A,B):-atom_codes(A,C),atom_codes(B,D),public_0_contains(C,D).
public_0_appendDiffList([],A,A).
public_0_appendDiffList([A|B],[A|C],D):-public_0_appendDiffList(B,C,D).
public_0_makeSetFromList(A,B):-public_0_foldLeft(A,[],lambda3_78,B).
public_0_setUnion(A,B,D):-append(A,B,C),public_0_makeSetFromList(C,D).
public_0_setDifference(A,B,C):-public_0_foldLeft(A,[],lambda3_79(B),C).
private_0_insertItem([],_,A,[A]):-!.
private_0_insertItem([A|D],B,C,[A|E]):-call_lambda2(B,C,A),private_0_insertItem(D,B,C,E),!.
private_0_insertItem([B|C],_,A,[A,B|C]).
public_0_sortItems(A,B,C,G):-public_0_map(A,lambda2_80(B),D),E=lambda2_81(C),public_0_foldLeft(D,[],lambda3_82(E),F),public_0_map(F,lambda2_83,G).
public_0_onFailure(A,_):-call_lambda0(A),!.
public_0_onFailure(_,A):-call_lambda0(A),!,fail.
public_0_yolo_UNSAFE_format_shim(A,B):-format(A,B).
public_0_duplicates(A,B):-public_0_foldLeft(A,public_0_pair([],[]),lambda3_84,public_0_pair(_,B)).
