let day12p1 (input : string) = 
  let lines = Stdlib.open_in input in
  let sz = 140 in
  let garden : string array array = Array.make_matrix sz sz "O" in
  let vis : int array array = Array.make_matrix sz sz 0 in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      garden.(i).(j) <- Char.escaped inp.[j];
    done
  done;
  (*floodfill*)
  let ans = ref 0 in
  let cur_area = ref 1 in
  let cur_per = ref 0 in
  let q : (int * int) Queue.t= Queue.create () in

  let floodfill (match_id : string) : unit =
    while not (Queue.is_empty q) do
      let curx, cury = Queue.pop q in
      let dir = [(0,1); (1,0); (-1,0); (0,-1)] in
      let rec iter_dir dir = 
        match dir with
        | (dx,dy) :: t -> if curx+dx >= 0 && curx+dx < sz && cury+dy >= 0 && cury+dy < sz && garden.(curx+dx).(cury+dy) = match_id then
          ((if (vis.(curx+dx).(cury+dy) = 0) then (vis.(curx+dx).(cury+dy) <- 1; Queue.push (curx+dx, cury+dy) q; incr cur_area)); iter_dir t) else (incr cur_per; iter_dir t)
        | [] -> ()
      in
      iter_dir dir
    done;
    ans := !ans + !cur_area * !cur_per;
    (*print_endline match_id;
    print_endline (string_of_int !cur_area);
    print_endline (string_of_int !cur_per);*)
    cur_area := 1;
    cur_per := 0;
  in
  for i=0 to sz-1 do
    for j=0 to sz-1 do
      if vis.(i).(j) = 0 then
        (vis.(i).(j) <- 1;
        Queue.add (i,j) q;
        floodfill garden.(i).(j);)
    done
  done;
  print_endline (string_of_int !ans)

let day12p2 (input : string) = 
  let lines = Stdlib.open_in input in
  let sz = 140 in
  let garden : string array array = Array.make_matrix sz sz "O" in
  let vis : int array array = Array.make_matrix sz sz 0 in
  let vis2 : int array array = Array.make_matrix sz sz 0 in
  let global_id : int ref = ref 1 in
  for i=0 to sz-1 do
    let inp = Stdlib.input_line lines in
    for j=0 to sz-1 do
      (*print_endline (Char.escaped inp.[j]);*)
      garden.(i).(j) <- Char.escaped inp.[j];
    done
  done;
  (*floodfill*)
  let ans = ref 0 in
  let cur_area = ref 0 in
  let cur_per = ref 0 in
  let cur_corners = ref 0 in
  let q : (int * int) Queue.t= Queue.create () in

  (*
  let check_bounds (i : int) (j : int) (match_id : string)  : bool = 
    i >= 0 && i < sz && j >= 0 && j < sz && garden.(i).(j) = match_id
  in*)

  let check_bounds (i : int) (j : int) (match_id : int)  : bool = 
    i >= 0 && i < sz && j >= 0 && j < sz && vis.(i).(j) = match_id
  in
  let conv_corner (match_id : int) (i,j : int * int) = 
    let mix_sum (dx : int) (dy : int) = if check_bounds (i + dx) (j + dy) (match_id) then 1 else 0 in
    [[mix_sum (1) (-1); mix_sum (1) (0); mix_sum (1) (1)];[mix_sum 0 (-1); mix_sum 0 0; mix_sum 0 1];[mix_sum (-1) (-1); mix_sum (-1) (0); mix_sum (-1) (1)]]
  in


  let check_corners (check : int list list) : int =
    (*same as matchid is 1, not same is 0*)
    let corner_count = ref 0 in
    (match check with
    | [[_; 0; 0]; [_;1; 0]; [_;_;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[0; 0; _]; [0;1; _]; [_;_;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; _; _]; [0;1; _]; [0;0;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; _; _]; [_;1; 0]; [_;0;0]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; 1; 0]; [_;1; 1]; [_;_;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[0; 1; _]; [1;1; _]; [_;_;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; _; _]; [1;1; _]; [0;1;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; _; _]; [_;1; 1]; [_;1;0]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; 0; 1]; [_;1; 0]; [_;_;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[1; 0; _]; [0;1; _]; [_;_;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; _; _]; [0;1; _]; [1;0;_]] -> incr corner_count
    | _ -> ());
    (match check with
    | [[_; _; _]; [_;1; 0]; [_;0;1]] -> incr corner_count
    | _ -> ());
    !corner_count
  in

  let floodfill (match_id : string) (mark : int) : unit =
    while not (Queue.is_empty q) do
      let curx, cury = Queue.pop q in
      let () = if vis.(curx).(cury) = 0 then (incr cur_area; cur_corners := !cur_corners + (check_corners (conv_corner (vis.(curx).(cury)) (curx,cury))); vis.(curx).(cury) <- mark) in
      let dir = [(0,1); (1,0); (-1,0); (0,-1)] in
      let rec iter_dir dir = 
        match dir with
        | (dx,dy) :: t -> if curx + dx >= 0 && curx + dx < sz && cury+dy >= 0 && cury+dy < sz && garden.(curx+dx).(cury+dy) = match_id then
          ((if (vis.(curx+dx).(cury+dy) = 0) then (vis.(curx+dx).(cury+dy) <- mark; Queue.push (curx+dx, cury+dy) q; incr cur_area; cur_corners := !cur_corners + (check_corners (conv_corner (vis.(curx).(cury)) (curx+dx,cury+dy))))); iter_dir t) else (incr cur_per; iter_dir t)
        | [] -> ()
      in
      iter_dir dir
    done;
    ans := !ans + !cur_area * !cur_corners;
    print_endline match_id;
    print_endline (string_of_int !cur_area);
    print_endline (string_of_int !cur_corners);
    cur_corners := 0;
    cur_area := 0;
    cur_per := 0;
  in
  for i=0 to sz-1 do
    for j=0 to sz-1 do
      if vis.(i).(j) = 0 then
        ((*vis.(i).(j) <- 1;*)
        Queue.add (i,j) q;
        floodfill garden.(i).(j) !global_id;
        incr global_id)
    done
  done;
  ans := 0;
  print_endline "EE";
  let floodfill_p2 (match_id : string) (mark : int) : unit =
    while not (Queue.is_empty q) do
      let curx, cury = Queue.pop q in
      let () = if vis2.(curx).(cury) = 0 then (incr cur_area; cur_corners := !cur_corners + (check_corners (conv_corner (vis.(curx).(cury)) (curx,cury))); vis2.(curx).(cury) <- mark) in
      let dir = [(0,1); (1,0); (-1,0); (0,-1)] in
      let rec iter_dir dir = 
        match dir with
        | (dx,dy) :: t -> if curx + dx >= 0 && curx + dx < sz && cury+dy >= 0 && cury+dy < sz && garden.(curx+dx).(cury+dy) = match_id then
          ((if (vis2.(curx+dx).(cury+dy) = 0) then (vis2.(curx+dx).(cury+dy) <- mark; Queue.push (curx+dx, cury+dy) q; incr cur_area; cur_corners := !cur_corners + (check_corners (conv_corner (vis.(curx).(cury)) (curx+dx,cury+dy))))); iter_dir t) else (incr cur_per; iter_dir t)
        | [] -> ()
      in
      iter_dir dir
    done;
    ans := !ans + !cur_area * !cur_corners;
    print_endline match_id;
    print_endline (string_of_int !cur_area);
    print_endline (string_of_int !cur_corners);
    cur_corners := 0;
    cur_area := 0;
    cur_per := 0;
  in
  for i=0 to sz-1 do
    for j=0 to sz-1 do
      if vis2.(i).(j) = 0 then
        ((*vis.(i).(j) <- 1;*)
        Queue.add (i,j) q;
        floodfill_p2 garden.(i).(j) !global_id;
        incr global_id)
    done
  done;
  print_endline (string_of_int !ans)



