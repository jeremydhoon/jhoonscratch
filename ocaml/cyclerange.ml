(*CycleRange, from a TopCoder problem. Works Correctly. *)

 let cycleRange startIters endIters r =
  let f x = r*.x*.(1.0-.x) in
  let _STARTX = 0.25 in
  let rec initialCycle remaining x =
    match remaining with
      | 0 -> x
      | _ -> initialCycle (remaining - 1) (f x)
  in
  let rec finalCycle remaining x ret =
    match remaining with
      | 0 -> ret
      | _ -> (
	let next = f x in
	finalCycle (remaining - 1) next (next :: ret))
  in
  let rec getRange lst sup inf = 
    match lst with
      | [] -> sup -. inf
      | _ -> (
	let curr = List.hd lst in
	getRange (List.tl lst) (max curr sup) (min curr inf))
  in
  getRange (finalCycle endIters (initialCycle startIters _STARTX) []) 0.0 r
;;

print_endline (string_of_float (cycleRange 200000 1000 (float_of_string Sys.argv.(1))));;
    