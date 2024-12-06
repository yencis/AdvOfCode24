let day06p1 (input : string) =
  let lines = Stdlib.open_in input in
  let sz = 130 in
  let word_search : string array array = Array.make_matrix sz sz "O" in
  let vis : int array array = Array.make_matrix sz sz 0 in
  let start_r = ref 0 in
  let start_c = ref 0 in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- Char.escaped inp.[j];
      if word_search.(i).(j) = "^" then (
        start_r := i;
        start_c := j;)
    done
  done;
  let spots = ref 0 in
  let cur_pos = ref (!start_r, !start_c) in
  let dir = ref (-1,0) in
  let in_board (i,j) =
    i >= 0 && i < sz && j >= 0 && j < sz in
  let turn_right (r,c) = 
    match r,c with
    | (-1,0) -> (0,1)
    | (0,1) -> (1,0)
    | (1,0) -> (0,-1)
    | (0,-1) -> (-1,0)
    | _ -> failwith "E"
  in
  
  try
    while in_board !cur_pos do 
      if vis.(fst !cur_pos).(snd !cur_pos) = 0 then (
        incr spots;
        vis.(fst !cur_pos).(snd !cur_pos) <- 1);
      (*print_endline (string_of_int (fst !cur_pos));
      print_endline (string_of_int (snd !cur_pos));*)
      (*cur_pos := (fst !cur_pos + fst !dir, snd !cur_pos + snd !dir);*)
      let temp_pos = (fst !cur_pos+fst !dir,snd !cur_pos + snd !dir) in
      match word_search.(fst temp_pos).(snd temp_pos) with
      | "#" -> dir := turn_right !dir;
      | _ -> cur_pos := (fst !cur_pos+fst !dir,snd !cur_pos + snd !dir);

      (*print_endline "Next";
      print_endline (string_of_int (fst !cur_pos));
      print_endline (string_of_int (snd !cur_pos));*)
    done;
  with Invalid_argument _ -> ();
  print_endline (string_of_int !spots)

let day06p2 (input : string) = 
  let lines = Stdlib.open_in input in
  let sz = 130 in
  let word_search : string array array = Array.make_matrix sz sz "O" in
  let val_pos : (int * int) list ref = ref [] in
  let vis : int array array = Array.make_matrix sz sz 0 in
  let dir_vis : (int * int) array array = Array.make_matrix sz sz (0,0) in
  let start_r = ref 0 in
  let start_c = ref 0 in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- Char.escaped inp.[j];
      if word_search.(i).(j) = "^" then (
        start_r := i;
        start_c := j;)
    done
  done;
  let spots = ref 0 in
  let cur_pos = ref (!start_r, !start_c) in
  let dir = ref (-1,0) in
  let in_board (i,j) =
    i >= 0 && i < sz && j >= 0 && j < sz in
  let turn_right (r,c) = 
    match r,c with
    | (-1,0) -> (0,1)
    | (0,1) -> (1,0)
    | (1,0) -> (0,-1)
    | (0,-1) -> (-1,0)
    | _ -> failwith "E"
  in
  
  try
    while in_board !cur_pos do 
      if vis.(fst !cur_pos).(snd !cur_pos) = 0 then (
        incr spots;
        vis.(fst !cur_pos).(snd !cur_pos) <- 1;
        dir_vis.(fst !cur_pos).(snd !cur_pos) <- !dir;
        val_pos := !cur_pos :: !val_pos);
      (*print_endline (string_of_int (fst !cur_pos));
      print_endline (string_of_int (snd !cur_pos));*)
      (*cur_pos := (fst !cur_pos + fst !dir, snd !cur_pos + snd !dir);*)
      let temp_pos = (fst !cur_pos+fst !dir,snd !cur_pos + snd !dir) in
      match word_search.(fst temp_pos).(snd temp_pos) with
      | "#" -> dir := turn_right !dir;
      | _ -> cur_pos := (fst !cur_pos+fst !dir,snd !cur_pos + snd !dir);

      (*print_endline "Next";
      print_endline (string_of_int (fst !cur_pos));
      print_endline (string_of_int (snd !cur_pos));*)
    done;
  with Invalid_argument _ -> ();

  (* usibng  vis array*)
  let pos = ref 0 in
  let simulate (block : int * int) : bool =
    word_search.(fst block).(snd block) <- "#";
    let new_vis : int array array = Array.make_matrix sz sz 0 in
    let new_dir_vis : (int * int) array array = Array.make_matrix sz sz (0,0) in
    let cur_pos = ref (!start_r, !start_c) in
    let dir = ref (-1,0) in
    let looped = ref false in
    let _ =
    try
      while in_board !cur_pos && not !looped do 
        if new_vis.(fst !cur_pos).(snd !cur_pos) = 1 && new_dir_vis.(fst !cur_pos).(snd !cur_pos) = !dir then
          looped := true;
        if new_vis.(fst !cur_pos).(snd !cur_pos) = 0 then (
          new_vis.(fst !cur_pos).(snd !cur_pos) <- 1;
          new_dir_vis.(fst !cur_pos).(snd !cur_pos) <- !dir);
        
        (*print_endline (string_of_int (fst !cur_pos));
        print_endline (string_of_int (snd !cur_pos));*)
        (*cur_pos := (fst !cur_pos + fst !dir, snd !cur_pos + snd !dir);*)
        let temp_pos = (fst !cur_pos+fst !dir,snd !cur_pos + snd !dir) in
        match word_search.(fst temp_pos).(snd temp_pos) with
        | "#" -> dir := turn_right !dir;
        | _ -> cur_pos := (fst !cur_pos+fst !dir,snd !cur_pos + snd !dir);

        (*print_endline "Next";
        print_endline (string_of_int (fst !cur_pos));
        print_endline (string_of_int (snd !cur_pos));*)
      done;
    with Invalid_argument _ -> (); in
    let _ = 
    word_search.(fst block).(snd block) <- "." in
    if !looped then
      true else false
  in 
  let () =
  for i=0 to (List.length !val_pos - 1) do
    (*print_endline (string_of_int !pos);*)
    let get_pos = List.nth !val_pos i in
    match simulate get_pos with
    | true -> incr pos
    | false -> ()
  done in ();

  print_endline (string_of_int !pos)
  
  