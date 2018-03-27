% Lengths, Starts, Pixels

check(_, Bit, [], []) :- Bit = false.
check(Idx, Bit, [S|SS], [L|LS]) :-
  (Idx #>= S,
  Idx #< S + L,
  Bit = true); check(Idx, Bit, SS, LS).
