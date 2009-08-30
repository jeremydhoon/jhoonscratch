 let splitByDivisors divisors n =
  let rec helper ret divs x =
    if divs == [] then x :: ret
    else
      let hd = List.hd divs in
      helper ((x mod hd) :: ret) (List.tl divs) (x / hd)
  in
  helper [] divisors n
;;

let whatTime seconds =
  let getPref current sep =
    if String.length current == 0 then String.create 0
    else current ^ sep
  in
  let rec helper ret lst sep =
    if lst == [] then ret
    else helper ((getPref ret sep) ^ (string_of_int (List.hd lst))) (List.tl lst) sep
  in
  helper "" (splitByDivisors [60; 60] seconds) ":"
;;

let whatTimes secLst =
  let rec helper lst =
    if lst == [] then ()
    else (
      print_endline (whatTime (List.hd lst));
      helper (List.tl lst))
  in
  helper secLst
;;

whatTimes [0; 3661; 5436; 86399]
