(*ORDER IS:
  FRONT
  BACK
  UP
  DOWN
  LEFT
  RIGHT
*)
type fraction = {d:int; n:int;};;
let string_of_fraction frac =
  match frac with
    | {d=_; n=0} -> "0"
    | {d=1; n=num} -> string_of_int num;
    | {d=denom; n=numerator} -> (string_of_int numerator) ^ "/" ^ (string_of_int denom)
;;

let rec euclid a b =
  if b = 0 then a
  else euclid b (a mod b)
;;
let simplify frac =
  let gcd = euclid frac.d frac.n in
  if gcd = 0 then {n=0; d=1;}
  else {d=frac.d/gcd; n=frac.n/gcd;}
;;
let addfractions a b = 
  let gcd = euclid a.d b.d in
  let a2b = b.d/gcd in
  let b2a = a.d/gcd in
  let lcm = b.d*b2a in
  simplify {n = a.n*a2b + b.n*b2a; d = lcm;}
;;
let divideFractions divisor frac = simplify {n = frac.n; d = frac.d*divisor;};;
let quarter = divideFractions 4;;
let cut one two three four =
   addfractions (addfractions (quarter one) (quarter two)) (addfractions (quarter three) (quarter four));;

let _F, _B, _U, _D, _L, _R = 0, 1, 2, 3, 4, 5;;
let udlr arr = cut arr.(_U) arr.(_D) arr.(_L) arr.(_R);;
let fblr arr = cut arr.(_F) arr.(_B) arr.(_L) arr.(_R);;
let udfb arr = cut arr.(_U) arr.(_D) arr.(_F) arr.(_B);;

let print_arr arr =
   let f i = print_endline (string_of_fraction(arr.(i))) in
   List.map f [0;1;2;3;4;5;]
;;
let float_of_fraction frac = float_of_int(frac.n) /. float_of_int(frac.d);;
let detectConvergence prev curr tolerance =
  let rec helper i ret =
    if i < 0 then ret
    else
      let sq a = a*.a in
      helper (i - 1) (ret +. (sq ((float_of_fraction prev.(i)) -. (float_of_fraction curr.(i)))))
    in
  let diff = helper 5 0.0 in
  diff < tolerance
;;

let snaug f b u d l r rounds =
  let thearray = [|{n=f;d=1;}; {n=b;d=1;}; {n=u;d=1;}; {n=d;d=1;}; {n=l;d=1;}; {n=r;d=1;};|] in
  let rec helper currarr oldarr rem =
    let inner arr remaining =
      if remaining == 0 then (
	arr.(2))
      else
	let _udlr,_fblr,_udfb = udlr arr, fblr arr, udfb arr in
	helper  [|_udlr; _udlr; _fblr; _fblr; _udfb; _udfb;|] (Some arr) (remaining - 1)
    in
    match oldarr with
      | Some a -> 
	  if detectConvergence a currarr 0.00000001 then currarr.(2) 
	  else inner currarr rem
      | None -> inner currarr rem
  in
  helper thearray None rounds
;;

let mysnaug = snaug 0 0 4 0 0 0 2;;
print_endline (string_of_fraction mysnaug);;