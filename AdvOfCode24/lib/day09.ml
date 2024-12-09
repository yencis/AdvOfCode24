let day09p1 (input : string) = 
  let lines = Stdlib.open_in input in
  let inp = Stdlib.input_line lines in
  (*let ids : int = (String.length inp + 1)/2 in*)
  let string_list : string list = String.fold_right (fun x a -> (Char.escaped x :: a)) inp [] in

  let peek_stack (data_stack : (int * int) list ) : int = 
    match data_stack with
    | h :: _ -> fst h
    | [] -> failwith "empty stack"
  in

  let pop_stack (data_stack : (int * int) list ) : (int * int) list =
    match data_stack with
    | h :: t -> if snd h = 1 then t else (fst h, snd h -1) :: t
    | [] -> failwith "empty stack"
  in
  let rec parse_input (string_list : string list) (cur_id : int) (init_data : int list) (data : (int * int) list) (is_data : bool)=
    match string_list with 
    | h :: t -> (if is_data then parse_input t (cur_id+1) ((List.init (int_of_string h) (fun _ -> cur_id)) @ init_data) ((cur_id, (int_of_string h)) :: data) (not is_data)
    else parse_input t (cur_id) ((List.init (int_of_string h) (fun _ -> -1)) @ init_data) (data) (not is_data))
    | [] -> (init_data, data)
  in 
  
  let (init_data, data) = parse_input string_list 0 [] [] true in
  let data_stack = data in
  let total_data = List.fold_right (fun x a-> snd x + a) data_stack 0 in
  (*let () = List.iter (fun x -> print_endline (string_of_int x)) init_data in*)
  (*let () = List.iter (fun x -> print_endline (string_of_int (fst x))) data_stack in*)
  let rec compress_data (init_data : int list) (data_stack : (int * int) list) (final_data : int list) (count : int) : int list= 
    if count = total_data then final_data else
    match init_data with
    | h :: t -> begin
      if h <> -1 then compress_data t data_stack (h :: final_data) (count + 1)
      else compress_data t (pop_stack data_stack) (peek_stack data_stack :: final_data) (count + 1)
    end
    | [] -> final_data
  in
  let compressed = compress_data (List.rev init_data) data_stack [] 0 |> List.rev in
  let rec check_sum (data : int list) (pos : int) (sum : int) = 
    match data with
    | h :: t -> check_sum t (pos + 1) (sum + pos * h)
    | [] -> sum
  in
  (*List.iter (fun x -> print_endline (string_of_int x)) compressed *)
  print_endline (check_sum compressed 0 0 |> string_of_int)

let day09p2 (input : string) = 
  let lines = Stdlib.open_in input in
  let inp = Stdlib.input_line lines in
  (*let ids : int = (String.length inp + 1)/2 in*)
  let string_list : string list = String.fold_right (fun x a -> (Char.escaped x :: a)) inp [] in


  let rec parse_input (string_list : string list) (cur_id : int) (init_data : (int * int) list) (data : (int * int) list) (is_data : bool)=
    match string_list with 
    | h :: t -> (if is_data then parse_input t (cur_id+1) ((cur_id, (int_of_string h)) :: init_data) ((cur_id, (int_of_string h)) :: data) (not is_data)
    else parse_input t (cur_id) ((-1, (int_of_string h)) :: init_data) (data) (not is_data))
    | [] -> (List.rev init_data, data)
  in 
  let init_data, data = parse_input string_list 0 [] [] true in

  let rec remove_data_with_id (storage : (int * int) list) (id : int) (acc : (int * int) list)=
    match storage with
    | h :: t -> if fst h = id then (List.rev acc) @ ((-1, snd h) :: t) else remove_data_with_id t id (h :: acc)
    | [] -> List.rev acc 
  in 

  let peek_stack (data_stack : (int * int) list ) : int = 
    match data_stack with
    | h :: _ -> fst h
    | [] -> failwith "empty stack"
  in

  let pop_stack (data_stack : (int * int) list ) : (int * int) list =
    match data_stack with
    | h :: t -> if snd h <= 1 then t else (fst h, snd h -1) :: t
    | [] -> failwith "empty stack"
  in

  let rec move_data (storage : (int * int) list) (data : int * int) (new_storage : (int * int) list): (int * int) list =
    match storage with
    | h :: t -> begin
      if fst h = -1 then begin
          if snd h > snd data then
            (List.rev new_storage) @ (data :: (-1, snd h - snd data) :: (remove_data_with_id t (fst data) []))
          else if snd h = snd data then
            (List.rev new_storage) @ (data :: (remove_data_with_id t (fst data) []))
          else move_data t data (h :: new_storage)
        end
      else 
        if fst h = fst data then (*end*)
          List.rev new_storage @ storage
        else
          move_data t data (h :: new_storage)
    end
    | [] -> List.rev new_storage @ storage (* no move *) 
  in
  let rec compress_all_data (datas : (int * int) list) (storage : (int * int) list) =
    match datas with 
    | h :: t -> compress_all_data t (move_data storage h [])
    | [] -> storage
  in
  let final_storage = compress_all_data data init_data in
  (*List.iter (fun x -> print_endline ((fst x |> string_of_int) ^ " " ^ (snd x |> string_of_int))) final_storage*)
  
  let rec check_sum (stack : (int * int) list) (pos : int) (sum : int) = 
    match stack with
    | h :: _ ->  if fst h = -1 then check_sum (pop_stack stack) (if snd h = 0 then pos else pos+1) sum else check_sum (pop_stack stack) (pos + 1) (sum + pos * (peek_stack stack))
    | [] -> sum
  in
  print_endline (string_of_int (check_sum final_storage 0 0)) 
(*let () = print_endline ((peek_stack stack |> string_of_int) ^ " " ^ (string_of_int pos) ^ " " ^ (string_of_int sum)) in*)