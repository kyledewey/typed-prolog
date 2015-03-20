module(io, [read_clauses_from_file/3], []).

use_module('common.pl', [], [option]).

datadef(stream, [A], [stream(A)]).
datadef(mode, [], [read_mode, write_mode]).

clausedef(yolo_UNSAFE_translate_mode, [A], [mode, A]).
yolo_UNSAFE_translate_mode(read_mode, read).
yolo_UNSAFE_translate_mode(write_mode, write).

clausedef(yolo_UNSAFE_open_file, [A], [atom, mode, stream(A)]).
yolo_UNSAFE_open_file(Filename, Mode, stream(Stream)) :-
        yolo_UNSAFE_translate_mode(Mode, OpenMode),
        open(Filename, OpenMode, Stream).

clausedef(yolo_UNSAFE_close_file, [A], [stream(A)]).
yolo_UNSAFE_close_file(stream(Stream)) :-
        close(Stream).

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
        yolo_UNSAFE_open_file(Filename, read_mode, Stream),
        read_clauses_from_stream(Stream, Translator, Result),
        yolo_UNSAFE_close_file(Stream).
