let day11p1 (input : string) = 
  let lines = Stdlib.open_in input in
  let inp : string list = Stdlib.input_line lines |> String.split_on_char ' ' in
  (*let answer_hash : int list list ref = ref [] in*)

  let split_string (str : string) : string * string =
    let part_a = String.sub str 0 (String.length str / 2) in
    let part_b = String.sub str (String.length str / 2) (String.length str / 2) |> int_of_string |> string_of_int in
    part_a, part_b
  in
  let rec simulate (blinks : int) (inc : int) (stones : string list) (ans_list : int list): string list * int list = 
    let rec step (stones : string list) (prev : string list) = 
      match stones with
      | h :: t -> begin
        match (h : string) with 
        | "0" -> step t ("1" :: prev)
        | x when (((String.length h) mod 2) = 0) -> let a,b = split_string x in step t (b :: a :: prev)
        | x -> step t ((string_of_int (int_of_string x * 2024)) :: prev)
      end
      | [] -> List.rev prev
    in
    match blinks with
    | 0 -> stones, ans_list
    | _ -> let s = step stones []  in simulate (blinks - 1) (inc + 1) (s) (List.length s :: ans_list)
  in
  (*for i=0 to 9 do

    let _, ans = simulate 35 0 [(string_of_int i)] [] in
    answer_hash := (ans :: !answer_hash);
  done*)
  
  let stones, _ = simulate 6 0 inp [] in
  let () = List.iter print_endline stones in
  print_endline (string_of_int (List.length stones))


let day11p2 (input : string) = 
  let lines = Stdlib.open_in input in
  let inp : string list = Stdlib.input_line lines |> String.split_on_char ' ' in
  let answer_hash : (int * int list) list ref = ref [] in

  let access_hash (id1: int) (id2: int) =
    let rec get_list (id1:int) (next:(int * int list) list) =
      if (List.hd next |> fst) <> id1 then get_list (id1) (List.tl next) else (List.hd next)
    in
    let hasht = get_list (id1) (!answer_hash) |> snd in
    let rec get_entry (id2 : int) (next : int list) = 
      if id2 <> 0 then get_entry (id2 - 1) (List.tl next) else (List.hd next)
    in
    get_entry id2 hasht
  in

  let split_string (str : string) : string * string =
    let part_a = String.sub str 0 (String.length str / 2) in
    let part_b = String.sub str (String.length str / 2) (String.length str / 2) |> int_of_string |> string_of_int in
    part_a, part_b
  in
  let rec step (stones : string list) (prev : string list) = 
    match stones with
    | h :: t -> begin
      match (h : string) with 
      | "0" -> step t ("1" :: prev)
      | x when (((String.length h) mod 2) = 0) -> let a,b = split_string x in step t (b :: a :: prev)
      | x -> step t ((string_of_int (int_of_string x * 2024)) :: prev)
    end
    | [] -> List.rev prev
  in
  let rec simulate (blinks : int) (inc : int) (stones : string list) (ans_list : int list): string list * int list = 
    
    match blinks with
    | 0 -> stones, ans_list
    | _ -> let s = step stones []  in simulate (blinks - 1) (inc + 1) (s) (List.length s :: ans_list)
  in
  for i=0 to 9 do

    let _, ans = simulate 40 0 [(string_of_int i)] [] in
    (*print_endline (string_of_int i);
    print_endline "_______________";
    List.iter print_endline stones;*)
    answer_hash := ((i, List.rev ans) :: !answer_hash);
  done;
  answer_hash := !answer_hash;
  print_endline "Precompute done";
  let sum = ref 0 in
  let goal = 75 in
  let rec bash_each (blinks : int) (inp_n : string list) : unit =
    (*print_endline (string_of_int blinks);*)
    if blinks = goal then (sum := (List.length inp_n) + !sum) else (
    match inp_n with
    | [] -> ()
    | h :: t -> begin
      (match h with
      | x when (String.length h <> 1) -> bash_each (blinks + 1) (step [x] [])
      | x -> if (goal - blinks) > 35 then bash_each (blinks + 1) (step [x] []) else sum := (access_hash (int_of_string x) (goal - blinks - 1)) + !sum);
      bash_each (blinks) t
    end)
  in bash_each 0 inp;
  print_endline (string_of_int !sum);
  (*print_endline (string_of_int (access_hash 1 (6-1)))*)

  
  
