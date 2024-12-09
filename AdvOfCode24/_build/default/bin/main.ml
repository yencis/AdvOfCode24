open AdvOfCode24.Day01
open AdvOfCode24.Day02
open AdvOfCode24.Day03
open AdvOfCode24.Day04
open AdvOfCode24.Day05
open AdvOfCode24.Day06
open AdvOfCode24.Day07
open AdvOfCode24.Day08
open AdvOfCode24.Day09

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
| "4","1" -> let _ = day04p1 file in ()
| "4","2" -> let _ = day04p2 file in ()
| "5","1" -> let _ = day05p1 file in ()
| "5","2" -> let _ = day05p2 file in ()
|"6","1" -> let _ = day06p1 file in ()
|"6","2" -> let _ = day06p2 file in ()
|"7","1" -> let _ = day07p1 file in ()
|"7","2" -> let _ = day07p2 file in ()
|"8","1" -> let _ = day08p1 file in ()
|"8","2" -> let _ = day08p2 file in ()
|"9","1" -> let _ = day09p1 file in ()
|"9","2" -> let _ = day09p2 file in ()
| _ -> ()
