let getpct points sum =
  let rec helper pcts remaining pts =
    if pts == [] then (pcts, remaining)
    else
      let curr = (100 * (List.hd pts))/sum in
      helper (curr :: pcts) (remaining - curr) (List.tl pts)
  in
  helper [] 100 points
;;

let findcutoff raw n =
  let sorted = List.rev Jdh.myMergeSort(raw) in
  Jdh.fwd n sorted
;;

let getdivision points =
  (*let points = List.stable_sort (fun a b -> b - a) pointlist in*)
  let sum = List.fold_left (fun a b -> a + b) 0 points in
  let raw,remaining = getpct points sum in
  let cutoff = findcutoff raw remaining in
  let rec helper ret lst =
    if lst == [] then ret
    else
      let curr = List.hd lst in
      helper ((curr + Jdh.int_of_bool(curr > cutoff)) :: ret) (List.tl lst)
  in
  helper [] raw 0
;;

(*let thepoints = [1;2;3;4;5];;*)
(*let thepoints = [485; 324; 263; 143; 470; 292; 304; 188; 100; 254; 296;
		  255; 360; 231; 311; 275; 93; 463; 115; 366; 197; 470;];;*)
let thepoints = [5;5;5;5;5;5;];;
print_endline (Jdh.print_list (getdivision thepoints) string_of_int ", ")
  
