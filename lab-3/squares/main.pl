:-
  [ library(clpfd)
  , library(dcg/basics)
  , library(pio)
  , library(main)
  , solver
  ].

% Printing

show_sym(1, '#').
show_sym(0, '.').

show_row(Stream, [A|R]) :- write(Stream, A), show_row(Stream, R).
show_row(Stream, []) :- nl(Stream).

show_board(File, Bits) :-
  open(File, write, Stream),
  maplist(maplist(show_sym), Bits, Res1),
  maplist(show_row(Stream), Res1),
  close(Stream).

% Loading from file

line([H|T]) --> integer(H), !, whites, line(T).
line([]) --> blanks.

lines(0, []) --> [].
lines(N, [H|T]) --> line(H), !, { N1 is N - 1 }, lines(N1, T).

input_grammar(Rows, Cols) -->
  integer(N), !,
  whites,
  integer(M), !, blanks,
  lines(N, Rows),
  lines(M, Cols),
  remainder(_).
  
parse_input(RowLengths, ColLengths) :-
  phrase_from_file(input_grammar(RowLengths, ColLengths), "zad_input.txt").



main(_) :-
  parse_input(Rows, Cols), !,
  check_game(Rows, Cols, Bits),
  show_board("zad_output.txt", Bits),
  halt.
