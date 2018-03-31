:- 
  [ library(clpfd)
  ].

check_lengths(_N, [], []).
check_lengths(N, [S], [L]) :- N #>= S + L.
check_lengths(N, [S1|[S2|SS]], [L|LS]) :-
  S1 + L #< S2,
  S1 + L #< N,
  check_lengths(N, [S2|SS], LS).

check_bit(Idx, Bit, Prev, [], []) :- Idx #>= Prev #==> #\ Bit.
check_bit(Idx, Bit, Prev, [S|SS], [L|LS]) :-
  End #= S + L,
  (Idx #>= S #/\ Idx #< End) #==> Bit,
  (Idx #< S #/\ Idx #>= Prev) #==> #\ Bit,
  check_bit(Idx, Bit, End, SS, LS).

check_bits_n(_, [], _, _).
check_bits_n(Idx, [B|Bits], Starts, Lengths) :-
  check_bit(Idx, B, 0, Starts, Lengths),
  Idx1 is Idx + 1,
  check_bits_n(Idx1, Bits, Starts, Lengths).

check_row(N, Bits, Starts, Lengths) :-
  check_lengths(N, Starts, Lengths),
  check_bits_n(0, Bits, Starts, Lengths).

inst_row(N, Lengths, Bits) :-
  length(Lengths, C),
  length(Starts, C),
  Starts ins 0..N,
  check_row(N, Bits, Starts, Lengths).

len(N, Ls) :- length(Ls, N).

dom_bit(N) :- N in 0..1.

check_game(RowLengths, ColLengths, Bits) :-
  length(RowLengths, NumRows),
  length(ColLengths, NumCols),
  length(Bits, NumRows),
  maplist(len(NumCols), Bits),
  maplist(maplist(dom_bit), Bits),
  transpose(Bits, BitCols),
  maplist(inst_row(NumCols), RowLengths, Bits),
  maplist(inst_row(NumRows), ColLengths, BitCols).
