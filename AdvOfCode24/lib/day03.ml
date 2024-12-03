
let day03p1 (input : string) =
  let sum = ref 0 in
  let regex = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let number = Str.regexp "[0-9]+" in
  let lines = Stdlib.open_in input in
  try
    let check = ref true in
    while in_channel_length lines > 0 do
      let inp : string = Stdlib.input_line lines in
      let pos = ref 0 in
      try
        while true do
          let pos_do = 10000000000 in
          let pos_dont =  1000000000 in 
          let pos_mul = Str.search_forward regex inp !pos in 
          let () = (if pos_do < pos_dont && pos_do < pos_mul then (check := true; pos := pos_do + 4) 
          else 
            (if (pos_dont < pos_mul) then (check := false; pos := pos_dont + 7) else
            match !check with
            | true -> let c = Str.matched_string inp in 
            let split_pos = Str.search_forward number c 0 in
            let val1 = Str.matched_string c in
            let val2 = (let _ = Str.search_forward number c (split_pos + String.length val1) in Str.matched_string c) in
            sum := !sum + (int_of_string val1 * int_of_string val2); pos := pos_mul + (String.length c) 
            | false ->  let c = Str.matched_string inp in pos := pos_mul + (String.length c) 
            )) in
          ()
        done
      with Not_found -> ()
    done;
  with End_of_file ->
    print_endline (string_of_int !sum)


let day03p2 (input : string) =
  let sum = ref 0 in
  let regex = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let do_reg = Str.regexp {|do()|} in
  let dont_reg = Str.regexp {|don't()|} in
  let number = Str.regexp "[0-9]+" in
  let lines = Stdlib.open_in input in
  try
    let check = ref true in
    while in_channel_length lines > 0 do
      let inp : string = Stdlib.input_line lines in
      let pos = ref 0 in
      try
        while true do
          let pos_do = try Str.search_forward do_reg inp !pos with Not_found -> 10000000000 in
          let pos_dont = try Str.search_forward dont_reg inp !pos with Not_found -> 1000000000 in 
          let pos_mul = Str.search_forward regex inp !pos in 
          let () = (if pos_do < pos_dont && pos_do < pos_mul then (check := true; pos := pos_do + 4) 
          else 
            (if (pos_dont < pos_mul) then (check := false; pos := pos_dont + 7) else
            match !check with
            | true -> let c = Str.matched_string inp in 
            let split_pos = Str.search_forward number c 0 in
            let val1 = Str.matched_string c in
            let val2 = (let _ = Str.search_forward number c (split_pos + String.length val1) in Str.matched_string c) in
            sum := !sum + (int_of_string val1 * int_of_string val2); pos := pos_mul + (String.length c) 
            | false ->  let c = Str.matched_string inp in pos := pos_mul + (String.length c) 
            )) in
          ()
        done
      with Not_found -> ()
    done;
  with End_of_file ->
    print_endline (string_of_int !sum)

