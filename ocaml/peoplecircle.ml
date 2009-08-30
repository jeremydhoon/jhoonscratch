type bit = {mutable b: bool};;

let print_order ord =
  let rec helper ret left =
    if left == [] then ret
    else (
      let curr = (List.hd left) in
      let symbol = match curr.b with
	| true -> "F"
	| false -> "M"
      in
      helper (ret ^ symbol) (List.tl left))
  in
  helper "" ord
;;

let order nm nf k =
  let size = nm + nf in
  let rec buildcircle ret reming = 
    if reming == 0 then ret
    else buildcircle ({b=false;} ::ret) (reming - 1)
  in
  let circle = buildcircle [] size in
  let rec helper fLeft start ptr count =
    if fLeft == 0 then start
    else (
      let rec advance start ptr =
	let next = List.tl ptr in
	if next == [] then
	    if (List.hd start).b then advance start start
	    else start
	else if (List.hd next).b == false then next
	else advance start next
      in
      if count == 1 then (
	(List.hd ptr).b <- true;
	helper (fLeft - 1) start (advance start ptr) k)
      else
	helper fLeft start (advance start ptr) (count - 1))
  in
  helper nf circle circle k
;;

let myOrder = order (int_of_string Sys.argv.(1))
		    (int_of_string Sys.argv.(2))
		    (int_of_string Sys.argv.(3));;
print_endline(print_order (myOrder));;
