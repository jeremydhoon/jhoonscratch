let returnPicks pos friends picks = 
  let delay1 = friends + pos in
  let delay2 = friends - pos in
  let rec helper ret rem isIncreasing prev =
    if (isIncreasing && (rem < delay1)) || (not isIncreasing && (rem < delay2)) then ret
    else
      match isIncreasing with
	| true -> helper ((prev + delay1) :: ret) (rem - delay1) false (prev + delay1)
	| false -> helper ((prev + delay2) :: ret) (rem - delay2) true (prev + delay2)
  in
  List.rev (helper [pos] (picks - pos) true pos)
;;

print_endline (Jdh.print_list (returnPicks 5 11 100) string_of_int ", ")