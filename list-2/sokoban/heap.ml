open Grid

module Make (Element: sig
  type t
  val compare: t -> t -> int
end) = struct
  module Elem = Element
  type heap = E | T of Elem.t * heap list

  exception Empty

  let empty = E
  let isEmpty h = match h with
    | E -> true
    | _ -> false

  let merge h1 h2 = match h1, h2 with
    | E, _ -> h2
    | _, E -> h1
    | T (x, hs1), T (y, hs2) ->
      if x <= y
      then T (x, h2 :: hs1)
      else T (y, h1 :: hs2)

  let insert x h = merge (T (x, [])) h

  let rec mergePairs h = match h with
    | [] -> E
    | [h] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (mergePairs hs)

  let deleteMin h = match h with
    | E -> raise Empty
    | T (x, hs) -> mergePairs hs

  let findMin h = match h with
    | E -> raise Empty
    | T (x, _) -> x
end
