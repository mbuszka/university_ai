open Grid

type elem = { key: int; value: state }

module Heap = Heap.Make (struct
  type t = elem
  let compare x y = compare x.key y.key
end)

let find_min xs = List.fold_left (fun m x -> if x < m then x else m) (List.hd xs) xs

let dist goals c =
  let cs = List.map (fun { x; y } -> (abs (x - c.x)) + (abs (y - c.y))) (Coords.elements goals) in
  find_min cs

let astar game init_state =
  let out = open_out "zad_output.txt" in
  let known = Hashtbl.create 1000000 in
  let rec aux q =
    if Heap.isEmpty q
    then ()
    else
      let { key = _; value = s } = Heap.findMin q in
      let q' = Heap.deleteMin q in
      let p = Hashtbl.find known s in
      (* print_int (Hashtbl.length known); print_newline (); *)
      if Coords.equal s.crates game.goals
      then (
        (* print_endline "finished"; *)
        List.iter (fun d -> output_string out (dir_to_string d)) (List.rev p);
        output_char out '\n')
      else
        let q'' = (directions |> List.fold_left (fun q d ->
          match move_guy game s d with
            | None -> q
            | Some state -> if Hashtbl.mem known state
              then q
              else (
                Hashtbl.add known state (d :: p);
                let ds = List.map (dist game.goals) (Coords.elements state.crates) in
                let d = List.fold_left (fun a x -> a + x) 0 ds in
                Heap.insert { key = d + (List.length p) + 1; value = state } q)) q') in
        aux q''
  in
    Hashtbl.add known init_state [];
    aux (Heap.insert { key = 0; value = init_state } Heap.empty)

let main =
  let strings = read_input "zad_input.txt" in
  let (game, state) = parse_state strings in
    astar game state
