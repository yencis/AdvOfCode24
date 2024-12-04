let check_xmas (word : string list) : bool =
  match word with
  | ["X";"M";"A";"S"] -> true
  | ["S";"A";"M";"X"] -> true
  | _ -> false

let add_buffer (buf : string list) (c : string) : string list =
  if List.length buf = 4 then
    List.tl buf @ [c]
  else
    buf @ [c]


let check_side_one (buff : string array array) : bool =
  match buff with
  | [|
    [|"M";_;_|];
    [|_;"A";_|];
    [|_;_;"S"|]
  |] -> true
  | [|
    [|"S";_;_|];
    [|_;"A";_|];
    [|_;_;"M"|]
  |] -> true
  | _ -> false

let check_side_two (buff : string array array) : bool =
  match buff with
  | [|
    [|_;_;"M"|];
    [|_;"A";_|];
    [|"S";_;_|]
  |] -> true
  | [|
    [|_;_;"S"|];
    [|_;"A";_|];
    [|"M";_;_|]
  |] -> true
  | _ -> false
  

let day04p1 (input : string) : unit =
  let lines = Stdlib.open_in input in
  let sz = 140 in
  let word_search : string array array = Array.make_matrix sz sz "O" in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- Char.escaped inp.[j]
    done
  done;
  
  let count = ref 0 in
  for i=0 to sz-1 do
    (*check all rows*)
    let word_buffer = ref [] in
    (*populate buffer*)
    for j=0 to 2 do
      word_buffer := !word_buffer @ [word_search.(i).(j)]
    done;
    for j=3 to sz-1 do
      
      word_buffer := add_buffer !word_buffer word_search.(i).(j);
      (*
      print_endline "Check";
      List.iter print_endline !word_buffer;
      *)
      if check_xmas !word_buffer then incr count;
    done
  done;
  
  
  for i=0 to sz-1 do
    (*check all cols*)
    let word_buffer = ref [] in
    (*populate buffer*)
    for j=0 to 2 do
      word_buffer := !word_buffer @ [word_search.(j).(i)]
    done;
    for j=3 to sz-1 do
      
      word_buffer := add_buffer !word_buffer word_search.(j).(i);
      if check_xmas !word_buffer then incr count;
    done
  done;
  
  
  (*check all diagonals starting from the top row and leading down*)
  let diag_check_row (start_row : int) (start_i : int) (end_i : int) (row_update : int -> int) (col_update : int -> int) : unit =
    (*print_endline (string_of_int start_i);
    print_endline (string_of_int end_i);*)
    for c=start_i to end_i do (*136 to guarantee there are always 4 letters*)
      let row = ref start_row in
      let col = ref c in
      let word_buffer = ref [] in
      (*populate buffer*)
      for _=0 to 2 do
        word_buffer := !word_buffer @ [word_search.(!row).(!col)];
        row := row_update !row;
        col := col_update !col;
      done;
      (*print_endline (string_of_int !row);
      print_endline (string_of_int !col);*)
      while !row < sz && !col < sz && !row >= 0 && !col >= 0 do
        
        word_buffer := add_buffer !word_buffer word_search.(!row).(!col);
        (*print_endline "Check";
        List.iter print_endline !word_buffer;*)
        if check_xmas !word_buffer then incr count;
        row := row_update !row;
        col := col_update !col;
      done
    done
  in
  diag_check_row 0 0 (sz-4) succ succ;
  diag_check_row 0 3 (sz-1) succ (fun x -> x-1);
  diag_check_row (sz-1) 1 (sz-4) (fun x -> x-1) succ;
  diag_check_row (sz-1) 3 (sz-2) (fun x -> x-1) (fun x -> x-1);
  
  print_endline (string_of_int !count)
  
let day04p2 (input : string) : unit =
  let lines = Stdlib.open_in input in
  let sz = 140 in
  let word_search : string array array = Array.make_matrix sz sz "O" in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- Char.escaped inp.[j]
    done
  done;
  let count = ref 0 in
  (*get all 3x3 (like a convolution filter)*)
  for i=0 to sz-3 do
    for j=0 to sz-3 do
      (* get the 3x3 with upper corner at this index*)
      let buffer = Array.make_matrix 3 3 "O" in
      for add_i =0 to 2 do
        for add_j=0 to 2 do
          buffer.(add_i).(add_j) <- word_search.(i+add_i).(j+add_j)
        done
      done;
      if check_side_one buffer && check_side_two buffer then incr count
    done
  done;
  print_endline (string_of_int !count)