let day10p1 (input : string) = 
  let lines = Stdlib.open_in input in
  let sz = 54 in
  let word_search : int array array = Array.make_matrix sz sz 0 in
  let vis : int array array = Array.make_matrix sz sz 0 in
  let trailheads : (int * int) list ref = ref [] in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- (int_of_string (Char.escaped inp.[j]));
      if word_search.(i).(j) = 0 then (
        trailheads := (i,j) :: !trailheads)
    done
  done;

  let refresh_array () = 
    for i=0 to sz-1 do
      for j=0 to sz-1 do
        (*print_endline (Char.escaped inp.[j]);*)
        vis.(i).(j) <- 0
      done
    done;
  in
  let dir = [(1,0); (-1,0); (0,1); (0,-1)] in
  let rec dfs (curx, cury : int * int) : int =
    if (word_search.(curx).(cury) = 9) then 
      if (vis.(curx).(cury) <> 1) then
        let () = vis.(curx).(cury) <- 1 in 1
      else 0
    else 
      let rec all_dir (dir : (int * int) list) (sum : int) : int =
        match dir with
        | (dx,dy) :: t -> if (curx + dx) >= 0 && (curx + dx) < sz && (cury + dy) >= 0 && (cury + dy) < sz && (word_search.(curx).(cury) + 1 = word_search.(curx + dx).(cury + dy)) then all_dir t (sum + dfs (curx+dx, cury+dy)) else all_dir t sum
        | [] -> sum
      in
      all_dir dir 0
    in
  let ans = List.fold_right (fun start acc ->  let x = acc + dfs start in let () = refresh_array () in x) !trailheads 0
  in print_endline (string_of_int ans)

(* Don't use a visited array *)
let day10p2 (input : string) = 
  let lines = Stdlib.open_in input in
  let sz = 54 in
  let word_search : int array array = Array.make_matrix sz sz 0 in
  let vis : int array array = Array.make_matrix sz sz 0 in
  let trailheads : (int * int) list ref = ref [] in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      word_search.(i).(j) <- (int_of_string (Char.escaped inp.[j]));
      if word_search.(i).(j) = 0 then (
        trailheads := (i,j) :: !trailheads)
    done
  done;

  let refresh_array () = 
    for i=0 to sz-1 do
      for j=0 to sz-1 do
        (*print_endline (Char.escaped inp.[j]);*)
        vis.(i).(j) <- 0
      done
    done;
  in
  let dir = [(1,0); (-1,0); (0,1); (0,-1)] in
  let rec dfs (curx, cury : int * int) : int =
    if (word_search.(curx).(cury) = 9) then 
      if (vis.(curx).(cury) <> 1) then
        let () = vis.(curx).(cury) <- 0 in 1 (* Visited array is invalidated here *)
      else 0
    else 
      let rec all_dir (dir : (int * int) list) (sum : int) : int =
        match dir with
        | (dx,dy) :: t -> if (curx + dx) >= 0 && (curx + dx) < sz && (cury + dy) >= 0 && (cury + dy) < sz && (word_search.(curx).(cury) + 1 = word_search.(curx + dx).(cury + dy)) then all_dir t (sum + dfs (curx+dx, cury+dy)) else all_dir t sum
        | [] -> sum
      in
      all_dir dir 0
    in
  let ans = List.fold_right (fun start acc ->  let x = acc + dfs start in let () = refresh_array () in x) !trailheads 0
  in print_endline (string_of_int ans)
