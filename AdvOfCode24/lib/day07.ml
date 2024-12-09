let day07p1 (input : string) = 
  let lines = Stdlib.open_in input in
  let sz = 850 in 
  let ans = ref 0 in
  for _ = 1 to sz do 
    let inp : string list = Stdlib.input_line lines |> String.split_on_char ' ' in
    let target : int = (List.hd inp) |> String.split_on_char ':' |> List.hd |> int_of_string in
    let values = List.tl inp |> List.map (int_of_string) in
    let rec check_values (ls : int list) (total : int) (target : int) : bool =
      match ls with 
      | h :: t -> check_values t (total + h) target || check_values t (total * h) target 
      | [] -> total = target
    in
    if check_values (List.tl values) (List.hd values) target then ans := !ans + target
  done;
  print_endline (string_of_int !ans)
  

let day07p2 (input : string) = 
  let lines = Stdlib.open_in input in
  let sz = 850 in (*850*)
  let ans = ref 0 in
  for _ = 1 to sz do 
    let inp : string list = Stdlib.input_line lines |> String.split_on_char ' ' in
    let target : int = (List.hd inp) |> String.split_on_char ':' |> List.hd |> int_of_string in
    let values = List.tl inp |> List.map (int_of_string) in
    let rec solve_w_ops (ls : int list) (total : int) (ops : string list) : int =
      match ls with 
      | h :: t -> (
        match List.hd ops with
        | "+" -> solve_w_ops t (total + h) (List.tl ops) 
        | "*" -> solve_w_ops t (total * h) (List.tl ops) 
        | "||" -> solve_w_ops t (int_of_string ((string_of_int total) ^ (string_of_int h))) (List.tl ops)
        (*| "||" -> int_of_string ((string_of_int total) ^ (string_of_int (solve_w_ops t h (List.tl ops))))*)
        | _ -> failwith "Error" )
      | [] -> total
    in
    let rec gen_w_ops (ls : int list) (og : int list) (ops : string list) (target : int): bool =
      match ls with
      | [h] -> 
        (*let () = print_endline "_______" in let () = List.iter print_endline ops in  *)
      let res = solve_w_ops (List.rev (h :: og) |> List.tl) (List.rev og |> List.hd) (ops) in
      (*let () = print_endline (string_of_int res) in 
      let () = List.iter (fun x -> print_endline (string_of_int x)) (List.rev (h :: og) |> List.tl) in
      let () = print_endline (string_of_int (List.rev og |> List.hd)) in *)
        res = target
      | h :: t ->  gen_w_ops t (h :: og) ("+" :: ops) target || gen_w_ops t (h::og) ("*" :: ops) target || gen_w_ops t (h::og) ("||" :: ops) target
      | [] -> failwith "error"
    in
    if gen_w_ops values [] [] target then ans := !ans + target
  done;
  print_endline (string_of_int !ans)
  