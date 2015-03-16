:- use_module('clauses_util.pl', [clausesFromFilesWithBuiltins/4]).
:- use_module('sanitizer.pl', [ensureProgram/3]).
:- use_module('typechecker.pl', [typecheckClauses/3]).
:- use_module('translator.pl', [translateClauses/2]).

processFiles(Files, TranslatedClauses) :-
        clausesFromFilesWithBuiltins(Files, DataDefs, ClauseDefs, Clauses), !,
        ensureProgram(DataDefs, ClauseDefs, Clauses), !,
        typecheckClauses(DataDefs, ClauseDefs, Clauses), !,
        translateClauses(Clauses, TranslatedClauses), !.

write_term(Term) :-
        copy_term(Term, Copy),
        numbervars(Copy, 0, _, [singletons(true)]),
        write_term(Copy, [numbervars(true)]),
        format('.~n').

processFiles(Files) :-
        processFiles(Files, TranslatedClauses),
        maplist(write_term, TranslatedClauses).
