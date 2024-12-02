open Batteries

let day02p1 (input:string) : unit = 
  let reports = ref 0 in
  let lines = Stdlib.open_in input in
  try
    while in_channel_length lines > 0 do
      let inp : int list = List.map int_of_string (Stdlib.input_line lines |> String.split_on_char ' ') in
      
      let rec check_safe (inp : int list) (low : int) (high : int) : bool =
        match inp with
        | h1 :: h2 :: t -> if h1 - h2 >= low && h1 - h2 <= high then check_safe (h2 :: t) low high else false
        | _ :: [] -> true
        | _ -> failwith "Death"
      in 
      if (check_safe inp 1 3 || check_safe inp (-3) (-1)) then
      incr reports else () 
    done;
  with End_of_file ->
    print_endline (string_of_int !reports)

let day02p2 (input:string) : unit =
  let reports = ref 0 in
  let lines = Stdlib.open_in input in
  try
    while in_channel_length lines > 0 do
      let inp : int list = List.map int_of_string (Stdlib.input_line lines |> String.split_on_char ' ') in
      let rec splice (pre : int list) (post : int list) (index : int) : int list =
        match post with
        | h::t -> if index = 0 then List.rev pre @ t else (splice (h::pre) t (index - 1))
        | [] -> failwith "check fail"
      in
      let rec check_safe (inp : int list) (low : int) (high : int) : bool =
        match inp with
        | h1 :: h2 :: t -> if h1 - h2 >= low && h1 - h2 <= high then check_safe (h2 :: t) low high else false
        | _ :: [] -> true
        | _ -> failwith "Death"
      in 
      let safe = ref false in begin
        for x = 0 to (List.length inp - 1) do
          let arr = splice [] inp x in
          if (check_safe arr 1 3 || check_safe arr (-3) (-1)) then
          safe := true else () 
        done;
        if !safe then incr reports else ();
      end
    done;
  with End_of_file ->
    print_endline (string_of_int !reports)