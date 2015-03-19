module(io, [yolo_UNSAFE_open_file/3, yolo_UNSAFE_close_file/1,
            yolo_UNSAFE_read_clause/3], [stream, mode]).

use_module('common.pl', [], [option]).

datadef(stream, [A], [stream(A)]).
datadef(mode, [], [read, write]).

clausedef(yolo_UNSAFE_open_file, [A], [atom, mode, stream(A)]).
yolo_UNSAFE_open_file(Filename, Mode, stream(Stream)) :-
        open(Filename, Mode, Stream).

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
