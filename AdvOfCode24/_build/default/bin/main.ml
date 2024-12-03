open AdvOfCode24.Day01
open AdvOfCode24.Day02
open AdvOfCode24.Day03

let () = 
let day = Sys.argv.(1) in
let part = Sys.argv.(2) in
let file = Sys.argv.(3) in
match day,part with 
| "1","1" -> let _ = day01p1 file in ()
| "1","2" -> let _ = day01p2 file in ()
| "2","1" -> let _ = day02p1 file in ()
| "2","2" -> let _ = day02p2 file in ()
| "3","1" -> let _ = day03p1 file in ()
| "3","2" -> let _ = day03p2 file in () 
| _ -> ()
