open Grid

type elem = { key: int; value: state }

module Heap = Heap.Make (struct
  type t = elem
  let compare x y = compare x.key y.key
end)

module Moves = Map.Make (struct
  type t = state
  let compare = compare
end)

let bfs game state =
  let known = Hashtbl.create 1000 in
  let paths = Hashtbl.create 1000 in
  let waiting = Queue.create () in
  let rec aux s = if Queue.is_empty waiting then s 
    else
      let coord = Queue.take waiting in
      let s' = if Hashtbl.mem known coord 
        then s
        else (
          Hashtbl.add known coord ();
          directions |> List.fold_left 
          (fun states d -> match move_guy game { guy = coord; crates = state.crates } d with
            | None -> states
            | Some s ->
              let c' = move coord d in
              let p = Hashtbl.find paths coord in
              if not (Hashtbl.mem paths c')
              then Hashtbl.add paths c' (d :: p)
              else ();
              Queue.add c' waiting;
              if not (Coords.equal s.crates state.crates)
              then Moves.add s (d :: p) states
              else states) s) in
      aux s'
  in
    Hashtbl.add paths state.guy [];
    Queue.add state.guy waiting;
    aux Moves.empty

let find_min xs = List.fold_left (fun m x -> if x < m then x else m) (List.hd xs) xs

let dist goals c =
  let cs = List.map (fun { x; y } -> (abs (x - c.x)) + (abs (y - c.y))) (Coords.elements goals) in
  find_min cs

let search game init =
  let out = open_out "zad_output.txt" in
  let known = Hashtbl.create 1000000 in
  let rec aux q =
    if Heap.isEmpty q
    then ()
    else
      let { key = _; value = s } = Heap.findMin q in
      let q' = Heap.deleteMin q in
      let path = Hashtbl.find known s in
      (* print_int (Hashtbl.length known); print_newline (); *)
      if Coords.equal s.crates game.goals
      then (
        (* print_endline "finished"; *)
        List.iter (fun d -> output_string out (dir_to_string d)) (List.rev path);
        output_char out '\n')
      else
        let states = Moves.bindings (bfs game s) in
        let q'' = (states |> List.fold_left (fun q (state, p) ->
            if Hashtbl.mem known state
              then q
              else (
                Hashtbl.add known state (List.append p path);
                let ds = List.map (dist game.goals) (Coords.elements state.crates) in
                let d = List.fold_left (fun a x -> a + x) 0 ds in
                Heap.insert { key = d + (List.length p) + 1; value = state } q)) q') in
        aux q''
  in
    Hashtbl.add known init [];
    aux (Heap.insert { key = 0; value = init } Heap.empty)

let main =
  let strings = read_input "zad_input.txt" in
  let (game, state) = parse_state strings in
    search game state
