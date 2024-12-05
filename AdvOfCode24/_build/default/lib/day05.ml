



let day05p1 (input : string) : unit =
  let lines = Stdlib.open_in input in
  let check = Hashtbl.create 100 in
  for _=1 to 1176 do (*1176*)
    (*let () = print_endline (string_of_int i) in*)
    let inp = Stdlib.input_line lines |> String.split_on_char '|' in
    let bef : int = (int_of_string (List.hd inp)) in
    let aft : int = (int_of_string (List.hd (List.tl inp))) in
    match Hashtbl.find_opt check bef with
    | Some ls -> Hashtbl.replace check bef (aft :: ls)
    | None -> Hashtbl.add check bef [aft]
    
  done;
  
  ignore (Stdlib.input_line lines);
  let ans = ref 0 in
  for _=1 to 197 do (*196*)
    let inp : int list = Stdlib.input_line lines |> String.split_on_char ',' |> List.map int_of_string |> List.rev in
    let check_memberships (a : int list) (b : int list) : bool = 
      List.is_empty (List.filter (fun x -> List.mem x a) b)
    in
    let rec check_inp (ls: int list) : bool =
      match ls with
      | h :: t -> (let vals_bef = Hashtbl.find_opt check h in
        match vals_bef with
        | Some vals ->
          if check_memberships vals t then check_inp t else false
        | None -> check_inp t)
      | [] -> true
    in
    if check_inp inp then ans := !ans + (List.nth inp (List.length inp / 2))
  done;
  print_endline (string_of_int !ans)

let swap (a : 'a list) (ind_a : int) (ind_b : int) : 'a list = (* swap elements in index ind_a and ind_b*)
let rec swap_replace (mog : 'a list) (cur_ind : int)  = (* always ind_a < ind_b *)
  match mog with 
  | h :: t -> 
    begin 
    if cur_ind = ind_a then List.nth a ind_b :: swap_replace t  (cur_ind + 1) 
    else if cur_ind = ind_b then List.nth a ind_a :: swap_replace t  (cur_ind + 1)
    else h :: swap_replace t  (cur_ind + 1) 
    end
  | [] -> []
in
swap_replace a 0

let day05p2 (input : string) : unit =
  let lines = Stdlib.open_in input in
  let check = Hashtbl.create 100 in
  for _=1 to 1176 do (*1176*)
    (*let () = print_endline (string_of_int i) in*)
    let inp = Stdlib.input_line lines |> String.split_on_char '|' in
    let bef : int = (int_of_string (List.hd inp)) in
    let aft : int = (int_of_string (List.hd (List.tl inp))) in
    match Hashtbl.find_opt check bef with
    | Some ls -> Hashtbl.replace check bef (aft :: ls)
    | None -> Hashtbl.add check bef [aft]
    
  done;
  
  ignore (Stdlib.input_line lines);
  let ans = ref 0 in
  for _=1 to 197 do (*197*)
    let inp : int list = Stdlib.input_line lines |> String.split_on_char ',' |> List.map int_of_string |> List.rev in
    (* continuously swap values*)
    

    let check_memberships (a : int list) (b : int list) : int list = 
      (List.filter (fun x -> List.mem x a) b)
    in
    let rec check_inp (ls: int list) : bool =
      match ls with
      | h :: t -> (let vals_bef = Hashtbl.find_opt check h in
        match vals_bef with
        | Some vals ->
          if List.is_empty (check_memberships vals t) then check_inp t else false
        | None -> check_inp t)
      | [] -> true
    in
    let rec swap_correct (a : int list) : int list =
      match a with
      | h :: t ->
        (let vals_bef = Hashtbl.find_opt check h in
        match vals_bef with
        | Some vals -> let members = check_memberships vals t in
          if List.is_empty members then h :: swap_correct t 
          else let swappable = List.hd members in 
            let ind_a = match List.find_index (fun x -> x = swappable) a with Some x -> x | None -> failwith "Unexpected" in
            let new_a = swap a ind_a 0 in
            swap_correct new_a 
        | None -> h :: swap_correct t)
      | [] -> []
    in
    if not (check_inp inp) then ans := !ans + (List.nth (swap_correct inp) (List.length inp / 2)) (* use a corrected list *)
  done;
  print_endline (string_of_int !ans)

