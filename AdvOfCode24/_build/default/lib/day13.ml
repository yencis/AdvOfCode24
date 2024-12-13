let day13p1 (input : string) = 
  let lines = Stdlib.open_in input in
  let queries = (1280/4) in

  
  let parse_with_sign (inp : string) (c : char) (trim_int : int)=
    let a = String.split_on_char c inp in
    (*let () = print_endline inp in*)
    match a with
    | _::b::[] -> int_of_string (String.sub b 0 (String.length b - trim_int))
    | _ -> failwith "Not valid parseable"
  in

  let get_tokens_three_four (inp : string list) =
    match inp with
    | [_;_;a;b] -> parse_with_sign a '+' (1), parse_with_sign b '+' 0
    | _ -> failwith "Not four input"
  in 

  
  let get_tokens_two_three (inp : string list) =
    match inp with
    | [_;a;b] -> parse_with_sign a '=' 1, parse_with_sign b '=' 0
    | _ -> failwith "Not 3 input"
  in 

  let tokens = ref 0 in
  for _=1 to queries do
    let ax,ay = Stdlib.input_line lines |> String.split_on_char ' ' |> get_tokens_three_four in 
    let bx,by = Stdlib.input_line lines |> String.split_on_char ' ' |> get_tokens_three_four in 
    let px,py = Stdlib.input_line lines |> String.split_on_char ' ' |> get_tokens_two_three in
    let px = px in
    let py = py in
   (* let () = print_endline (string_of_int ax) in
    let () = print_endline (string_of_int ay) in
    let () = print_endline (string_of_int bx) in
    let () = print_endline (string_of_int by) in
    let () = print_endline (string_of_int px) in
    let () = print_endline (string_of_int py) in*)
    let _ = Stdlib.input_line lines in
    (*let a = [[ax;bx];[ay;by]] in *)
    let deter = ax * by - ay * bx in
    (*let () = print_endline (string_of_int deter) in*)
    if deter <> 0 then
      let a = (by * px + (-bx) * py) / deter in
      let b = ((-ay) * px + ax * py) / deter in
      (*let () = print_endline (string_of_int a) in
      let () = print_endline (string_of_int b) in*)
      (*check remainders*)
      let c = (by * px + (-bx) * py) mod deter in
      let d = ((-ay) * px + ax * py) mod deter in
      if c = 0 && d = 0 then
        tokens := !tokens + a * 3 + b
  done;
  print_endline (string_of_int !tokens)



let day13p2 (input : string) = 
  let lines = Stdlib.open_in input in
  let queries = (1280/4) in

  
  let parse_with_sign (inp : string) (c : char) (trim_int : int)=
    let a = String.split_on_char c inp in
    (*let () = print_endline inp in*)
    match a with
    | _::b::[] -> int_of_string (String.sub b 0 (String.length b - trim_int))
    | _ -> failwith "Not valid parseable"
  in

  let get_tokens_three_four (inp : string list) =
    match inp with
    | [_;_;a;b] -> parse_with_sign a '+' (1), parse_with_sign b '+' 0
    | _ -> failwith "Not four input"
  in 

  
  let get_tokens_two_three (inp : string list) =
    match inp with
    | [_;a;b] -> parse_with_sign a '=' 1, parse_with_sign b '=' 0
    | _ -> failwith "Not 3 input"
  in 

  let tokens = ref 0 in
  for _=1 to queries do
    let ax,ay = Stdlib.input_line lines |> String.split_on_char ' ' |> get_tokens_three_four in 
    let bx,by = Stdlib.input_line lines |> String.split_on_char ' ' |> get_tokens_three_four in 
    let px,py = Stdlib.input_line lines |> String.split_on_char ' ' |> get_tokens_two_three in
    let px = px + 10000000000000  in
    let py = py + 10000000000000 in
    (* let () = print_endline (string_of_int ax) in
    let () = print_endline (string_of_int ay) in
    let () = print_endline (string_of_int bx) in
    let () = print_endline (string_of_int by) in
    let () = print_endline (string_of_int px) in
    let () = print_endline (string_of_int py) in*)
    let _ = Stdlib.input_line lines in
    (*let a = [[ax;bx];[ay;by]] in *)
    let deter = ax * by - ay * bx in
    (*let () = print_endline (string_of_int deter) in*)
    if deter <> 0 then
      let a = (by * px + (-bx) * py) / deter in
      let b = ((-ay) * px + ax * py) / deter in
      (*let () = print_endline (string_of_int a) in
      let () = print_endline (string_of_int b) in*)
      (*check remainders*)
      let c = (by * px + (-bx) * py) mod deter in
      let d = ((-ay) * px + ax * py) mod deter in
      if c = 0 && d = 0 then
        tokens := !tokens + a * 3 + b
  done;
  print_endline (string_of_int !tokens)



