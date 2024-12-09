open Set

module Pair : (OrderedType with type t = int * int) = struct
  type t = int * int

  let compare (a : t) (b : t) : int = 
    if (fst a - fst b = 0) then
      snd a - snd b
    else 
      fst a - fst b

end


let subtract (a : Pair.t) (b : Pair.t) =

  (fst a - fst b, snd a - snd b)

let add (a : int * int) (b : int * int) = 
  (fst a + fst b, snd a + snd b)

module PairSet = Set.Make (Pair)

let day08p1 (input : string) = 

  let lines = Stdlib.open_in input in
  let sz = 50 in
  let word_search : string array array = Array.make_matrix sz sz "O" in
  let antennas : (string, (int * int) list) Hashtbl.t = Hashtbl.create 16 in
  let signals : string list ref = ref [] in
  let antinodes = ref PairSet.empty in

  let check_pair_good (a : Pair.t) = 
    fst a >= 0 && snd a >= 0 && fst a < sz && snd a < sz 
  in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- Char.escaped inp.[j];
      if word_search.(i).(j) <> "." then
        match Hashtbl.find_opt antennas (word_search.(i).(j)) with
      | Some ls -> Hashtbl.replace antennas word_search.(i).(j) ((i,j) :: ls)
      | None -> Hashtbl.add antennas word_search.(i).(j) [(i,j)]; signals := (word_search.(i).(j) :: !signals)
    done
  done;
  let rec check_all_pairs (pairs : (int * int) list) : unit=
    match pairs with 
    | h :: t -> 
      (let rec make_pair (ls : (int * int) list) =
        match ls with
        | [] -> ()
        | h_x :: t_x ->( let dist = subtract h_x h in 
        if check_pair_good (subtract h dist) then
        antinodes := PairSet.add (subtract h dist) !antinodes; 
        if check_pair_good (add h_x dist) then
        antinodes := PairSet.add (add h_x dist) !antinodes; 
        make_pair t_x)
      in make_pair t); check_all_pairs t;
    | [] -> ()
    in
  let rec signal_check signals = 
    match signals with
    | h :: t -> check_all_pairs (Hashtbl.find antennas h); signal_check t;
    | [] -> ()
  in signal_check !signals;
  print_endline (List.length (PairSet.elements !antinodes) |> string_of_int)

let day08p2 (input : string) = 

  let lines = Stdlib.open_in input in
  let sz = 50 in
  let word_search : string array array = Array.make_matrix sz sz "O" in
  let antennas : (string, (int * int) list) Hashtbl.t = Hashtbl.create 16 in
  let signals : string list ref = ref [] in
  let antinodes = ref PairSet.empty in

  let check_pair_good (a : Pair.t) = 
    fst a >= 0 && snd a >= 0 && fst a < sz && snd a < sz 
  in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- Char.escaped inp.[j];
      if word_search.(i).(j) <> "." then
        match Hashtbl.find_opt antennas (word_search.(i).(j)) with
      | Some ls -> Hashtbl.replace antennas word_search.(i).(j) ((i,j) :: ls)
      | None -> Hashtbl.add antennas word_search.(i).(j) [(i,j)]; signals := (word_search.(i).(j) :: !signals)
    done
  done;
  
  let rec add_many_pairs (func) (pair) (interval) : unit =
    match check_pair_good (func pair interval) with
    | true -> antinodes := PairSet.add (func pair interval) !antinodes; add_many_pairs (func) (func pair interval) (interval)
    | false -> ()
  in


  let rec check_all_pairs (pairs : (int * int) list) : unit=
    match pairs with 
    | h :: t -> 
      (let rec make_pair (ls : (int * int) list) =
        match ls with
        | [] -> ()
        | h_x :: t_x ->( let dist = subtract h_x h in 
        add_many_pairs subtract h dist;
        add_many_pairs add h dist;
        (*if check_pair_good (subtract h dist) then
        antinodes := PairSet.add (subtract h dist) !antinodes; *)
        add_many_pairs add h_x dist;
        add_many_pairs subtract h_x dist;
        make_pair t_x)
      in make_pair t); check_all_pairs t;
    | [] -> ()
    in
  let rec signal_check signals = 
    match signals with
    | h :: t -> check_all_pairs (Hashtbl.find antennas h); signal_check t;
    | [] -> ()
  in signal_check !signals;
  print_endline (List.length (PairSet.elements !antinodes) |> string_of_int);
  (*List.iter (fun (a,b) -> (string_of_int a |> print_endline);(string_of_int b |> print_endline)) (PairSet.elements !antinodes)*)
