type coord = { x: int; y: int }
type obj = Guy | Crate
type field = Field of obj | Wall
type dir = U | D | L | R

module Coords = Set.Make(struct
  type t = coord
  let compare = compare
end)

type state = { guy: coord; crates: Coords.t }
type game = { walls: Coords.t; goals: Coords.t }


exception IllegalMove

let dir_to_string d = match d with
  | U -> "U"
  | D -> "D"
  | L -> "L"
  | R -> "R"

let directions = [ U; D; L; R ]

let string_to_list str =
  let rec loop i limit =
    if i = limit then []
    else (String.get str i) :: (loop (i + 1) limit)
  in
  loop 0 (String.length str)

let fold_lefti f acc ls = let
  (_, res) = List.fold_left (fun (i, a) x -> (i + 1, f i a x)) (0, acc) ls
  in res

let is_wall c = c = 'W'

let is_goal c = match c with
  | 'G' | '+' | '*' -> true
  | _ -> false

let is_guy c = c = '+' || c = 'K'

let is_crate c = c = 'B' || c = '*'

let find_coords p strings =
  fold_lefti 
    (fun y acc s -> 
      fold_lefti 
        (fun x a c -> if p c then { x; y } :: a else a)
        acc (string_to_list s))
    [] strings

let parse_state strings =
  let guy = List.hd (find_coords is_guy strings) in
  let crates = Coords.of_list (find_coords is_crate strings) in
  let goals = Coords.of_list (find_coords is_goal strings) in
  let walls = Coords.of_list (find_coords is_wall strings) in
  ({ walls; goals }, { guy; crates })

let read_input file =
  let ch = open_in file in
  let rec aux () = try
    let s = input_line ch in s :: aux () with
    | End_of_file -> []
  in aux ()

let map_opt f opt = match opt with
  | Some x -> Some (f x)
  | None -> None

let move c d = match d with
  | U -> { c with y = c.y - 1 }
  | D -> { c with y = c.y + 1 }
  | L -> { c with x = c.x - 1 }
  | R -> { c with x = c.x + 1 }

let move_crate game state coord dir =
  let c = move coord dir in
  if Coords.mem c game.walls then None
  else if Coords.mem c state.crates then None
  else Some (Coords.add c (Coords.remove coord state.crates))

let move_guy game state dir =
  let c = move state.guy dir in
  let crates = 
    if Coords.mem c game.walls then None
    else if Coords.mem c state.crates then move_crate game state c dir
    else Some state.crates
  in map_opt (fun crates -> { guy = c; crates = crates }) crates


let rec cat_options l = match l with
  | None :: xs -> cat_options xs
  | Some x :: xs -> x :: cat_options xs
  | [] -> []
