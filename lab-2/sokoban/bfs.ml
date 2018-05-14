open Grid

let bfs game init_state =
  let out = open_out "zad_output.txt" in
  let known = Hashtbl.create 1000000 in
  let waiting = Queue.create () in
  let rec aux () = 
    if Queue.is_empty waiting
    then ()
    else
      let s = Queue.take waiting in
      let p = Hashtbl.find known s in
      (* print_int (Hashtbl.length known); print_newline (); *)
      if Coords.equal s.crates game.goals
      then (
        (* print_endline "finished"; *)
        List.iter (fun d -> output_string out (dir_to_string d)) (List.rev p);
        output_char out '\n')
      else
        (directions |> List.iter (fun d ->
          match move_guy game s d with
            | None -> ()
            | Some state -> if Hashtbl.mem known state
              then ()
              else (
                Hashtbl.add known state (d :: p);
                Queue.add state waiting));
        aux ())
  in
    Hashtbl.add known init_state [];
    Queue.add init_state waiting;
    aux ()

let main =
  let strings = read_input "zad_input.txt" in
  let (game, state) = parse_state strings in
    bfs game state
