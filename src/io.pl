module(io, [read_clauses_from_file/3, writeClauses/2], []).

use_module('common.pl', [forall/2], [option]).

datadef(stream, [A], [stream(A)]).
datadef(mode, [], [read_mode, write_mode]).

clausedef(yolo_UNSAFE_translate_mode, [A], [mode, A]).
yolo_UNSAFE_translate_mode(read_mode, Op) :- Op = read, !.
yolo_UNSAFE_translate_mode(write_mode, Op) :- Op = write, !.

clausedef(yolo_UNSAFE_open_file, [A], [atom, mode, stream(A)]).
yolo_UNSAFE_open_file(Filename, Mode, stream(Stream)) :-
        yolo_UNSAFE_translate_mode(Mode, OpenMode),
        open(Filename, OpenMode, Stream).

clausedef(yolo_UNSAFE_close_file, [A], [stream(A)]).
yolo_UNSAFE_close_file(stream(Stream)) :-
        close(Stream).

clausedef(withOpenStream, [A], [atom, mode, relation([stream(A)])]).
withOpenStream(Filename, Mode, CallThis) :-
        yolo_UNSAFE_open_file(Filename, Mode, Stream),
        ((call(CallThis, Stream), yolo_UNSAFE_close_file(Stream), !);
         (yolo_UNSAFE_close_file(Stream), fail)).

clausedef(yolo_UNSAFE_read_clause, [A, B, C], [stream(A), relation([B, C]), option(C)]).
yolo_UNSAFE_read_clause(stream(Wrapped), Translator, Translated) :-
        read_clause(Wrapped, RawClause, []),
        (RawClause == end_of_file ->
            (Translated = none);
            (call(Translator, RawClause, TranslatedClause),
             Translated = some(TranslatedClause))).

clausedef(read_clauses_from_stream, [A, B, C], [stream(A), relation([B, C]), list(C)]).
read_clauses_from_stream(Stream, Translator, Result) :-
        Helper = lambda([Accum],
            (yolo_UNSAFE_read_clause(Stream, Translator, OpResult),
             (OpResult = some(Cur) ->
                 (Accum = [Cur|Rest],
                  call(Helper, Rest));
                 (Accum = [])))),
        call(Helper, Result).

clausedef(read_clauses_from_file, [A, B], [atom, relation([A, B]), list(B)]).
read_clauses_from_file(Filename, Translator, Result) :-
        withOpenStream(Filename, read_mode,
                       lambda([Stream],
                              read_clauses_from_stream(Stream, Translator, Result))).

clausedef(yolo_UNSAFE_write_clause, [A, B], [A, stream(B)]).
yolo_UNSAFE_write_clause(Clause, stream(Stream)) :-
        copy_term(Clause, Copy),
        numbervars(Copy, 0, _, [singletons(true)]),
        write_term(Stream, Copy, [numbervars(true), quoted(true)]),
        format(Stream, '.~n', []).

clausedef(writeClauses, [A], [list(A), atom]).
writeClauses(Clauses, Filename) :-
        withOpenStream(
            Filename, write_mode,
            lambda([Stream],
                   forall(Clauses,
                          lambda([Clause], yolo_UNSAFE_write_clause(Clause, Stream))))).
