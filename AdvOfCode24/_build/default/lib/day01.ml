open Batteries

let day01p1 (input:string) : unit =
  let heap1 = ref BatHeap.empty in
  let heap2 = ref BatHeap.empty in
  let lines = Stdlib.open_in input in
  try
    while in_channel_length lines > 0 do
      match Stdlib.input_line lines |> String.split_on_char ' ' with
      |[a;_;_;b] -> heap1 := BatHeap.insert !heap1 (int_of_string a); heap2 := BatHeap.insert !heap2 (int_of_string b); 
      |b -> List.iter print_endline b; failwith "Err"
    done;
  with End_of_file ->
    let dist = ref 0 in
    while BatHeap.size !heap1 > 0 do
      BatHeap.(
      dist := !dist + abs (find_min !heap1 - find_min !heap2);
      heap1 := del_min !heap1;
      heap2 := del_min !heap2)
    done;
    print_endline (string_of_int !dist)

let day01p2 (input:string) : unit =
  let arr1 : (int list ref) = ref [] in
  let ht = Hashtbl.create 32 in
  let lines = Stdlib.open_in input in
  try
    while in_channel_length lines > 0 do
      match Stdlib.input_line lines |> String.split_on_char ' ' with
      |[a;_;_;b] -> arr1 := int_of_string a :: !arr1;
        let z = int_of_string b in Hashtbl.(if mem ht z then replace ht z ((find ht z) + 1) else add ht z 1); 
      |b -> List.iter print_endline b; failwith "Err"
    done;
  with End_of_file ->
    let dist = List.fold (fun a b -> (a + let mult = match Hashtbl.find_option ht b with Some x -> x | None -> 0 in mult * b)) 0 !arr1 in 
    print_endline (string_of_int dist)