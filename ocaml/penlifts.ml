(*Result of looking for an intersection. Either no intersection occurs,
an intersection happens at (x, y),
or the two segments together form the longer segment (x1,y1,x2,y2). *)
type intersection_t = NoIntersection | Intersection of int * int | Overlap of int * int * int * int;;

let stringifyIntersection intersection =
    match intersection with
      | Intersection(a,b) -> ((string_of_int a) ^ " " ^ (string_of_int b))
      | NoIntersection -> "No intersection."
      | Overlap(a,b,c,d) -> "Overlapping segments reduce to segment " ^ (string_of_int a) ^ " " ^ (string_of_int b) ^ " " ^ (string_of_int c) ^ " " ^ (string_of_int d)
;;

let bool_of_intersection_t isect =
  match isect with
    | NoIntersection -> false
    | Intersection(_,_) -> true
    | Overlap(_,_,_,_) -> false
;;

let checkBounds sA sB (x,y) =
  let checkAxis x1 x2 x = (x1 <= x && x2 >= x) || (x1 >= x && x2 <= x) in
  let checkX seg x =
    match seg with
      | (x1, _, x2, _) -> checkAxis x1 x2 x
  in
  let checkY seg y =
    match seg with
      | (_, y1, _, y2) -> checkAxis y1 y2 y
  in
  if (checkX sA x) && (checkY sA y) && (checkX sB x) && (checkY sB y) then Intersection(x,y)
  else NoIntersection
;;

let checkSlopes sA sB =
  let (ax1, ay1, ax2, ay2) = sA in
  let (bx1, by1, bx2, by2) = sB in
  let min4 a b c d = min (min a b) (min c d) in
  let max4 a b c d = max (max a b) (max c d) in
  
  let theOverlap = Overlap(min4 ax1 ax2 bx1 bx2, min4 ay1 ay2 by1 by2, max4 ax1 ax2 bx1 bx2, max4 ay1 ay2 by1 by2) in
  let doCheck pair = bool_of_intersection_t (checkBounds sA sB pair) in
  if (ax2 - ax1 == 0) && (bx2 - bx1 == 0) then
    if (ax1 == bx1 && ((doCheck (ax1, ay1)) || (doCheck (ax1, by1)) || (doCheck (ax1, by2)))) then
      theOverlap
    else NoIntersection
  else
    if (ay1 == by1 && ((doCheck (ax1, ay1)) || (doCheck (ax2, ay1)) || (doCheck (bx1, ay1)))) then
      theOverlap
    else NoIntersection
;;

(*get intersection of segment 1 and segment 2*)
let getIntersection sA sB =
  let (ax1, ay1, ax2, ay2) = sA in
  let (bx1, by1, bx2, by2) = sB in
  let det a b c d = (a * d) - (b * c) in
  let denom = det (ax1 - ax2) (ay1 - ay2) (bx1 - bx2) (by1 - by2) in
  let aDet = det ax1 ay1 ax2 ay2 in
  let bDet = det bx1 by1 bx2 by2 in
  let makeNumerator ul ur bl br =  (det aDet (ul - ur) bDet (bl - br)) / denom in
  if denom != 0 then checkBounds sA sB (makeNumerator ax1 ax2 bx1 bx2, makeNumerator ay1 ay2 by1 by2)
  else checkSlopes sA sB
;;  

let print_int_int_list l =
  let rec helper lst =
    if (List.length lst > 0) then
      let (x,y) = List.hd lst in
      let next = 
	print_endline ("(" ^ string_of_int(x) ^ " " ^ string_of_int(y) ^ ")");
	helper (List.tl lst)
      in
      next
    else ()
  in
  helper l
;;

exception NoIntersections of string;;
class edge x1 y1 x2 y2 =
  object (self)
    val mutable isect_list = ([] : (int * int) list)
    val mutable horiz = (None : bool option)
    method findIntersections allIntersections =
      let between lb mid ub = (mid >= lb && mid <= ub) || (mid <= lb && mid >= ub) in
      let rec helper ret lst =
	let app r pair = 
	  if (List.exists (fun p -> (fst pair) == (fst p) && (snd pair) == (snd p)) r == false) then
	    helper (pair::r) (List.tl lst)
	  else helper r (List.tl lst)
	in
	if (List.length lst <= 0) then ret else
	let (x,y) = List.hd lst in
	match horiz with
	| Some(truth) ->
	  if (truth && (y1 - y == 0) && (between x1 x x2)) then app ret (x,y)
	  else if (truth == false && (x1 - x == 0) && (between y1 y y2)) then app ret (x,y)
	  else helper ret (List.tl lst)
	| None -> raise (NoIntersections "Control should not reach here.")
      in
      horiz <- Some(y1 - y2 == 0);
      isect_list <- (helper [(x1,y1); (x2,y2)] allIntersections)
    method getNearestIntersection x y =
      let rec helper best lst =
	let (myX,myY) = List.hd lst in
	let iabs n = if n < 0 then -n else n in
	let test dist =
	  let adist = iabs(dist) in
	  match best with
	    | None -> Some(adist)
	    | Some(d) -> Some(min d adist)
	in
	if List.length lst <= 0 then best else
	match horiz with
	  | Some a ->
	    if a then helper (test (x - myX)) (List.tl lst)
	    else helper (test (y - myY)) (List.tl lst)
	  | None -> raise (NoIntersections "Called getNearestIntersection without populating intersections.")
      in
      helper None isect_list
    method createHopsList =
      match horiz with
	| Some(truth) ->
	  let hSortFunc pair1 pair2 = fst(pair1) - fst(pair2) in
	  let vSortFunc pair1 pair2 = snd(pair1) - snd(pair2) in
	  let rec helper ret behind ahead =
	    if (List.length ahead > 0) then helper ((List.hd behind, List.hd ahead) :: ret) (List.tl behind) (List.tl ahead)
	    else ret
	  in
	  let doLaunch lst = if (List.length lst > 0) then helper [] lst (List.tl lst) else [] in
	  if truth then doLaunch (List.sort hSortFunc isect_list) else doLaunch (List.sort vSortFunc isect_list)
	| None -> raise (NoIntersections "Called createHopsList without populating intersections.")
    method printIsectList = print_int_int_list isect_list
  end;;

(*
let rec createGraph allSegments segments tbl =
  if List.length segments <= 0 then tbl
  else createGraph allSegments (List.tl segments) (insertSeg tbl allSegments (List.hd segments))
;;
*)
let split_string s del =
  let rec helper ret s delim i =
    let findNext =
      let len = (String.length s) in
      if (i < len) then
	if (String.contains_from s i delim) then String.index_from s i delim else len
      else -1
    in
    let moveTo subret index =
      if (index > -1) then helper (String.sub s i (index - i) :: subret) s delim (index + 1)
      else subret
    in
    moveTo ret findNext
  in
  List.rev (helper [] s del 0)
;;
let parseSegments s = 
  let quads = split_string s '\"' in
  let rec helper ret lst =
    let rec getFourNums numStrLst =
      let nth n = int_of_string (List.nth numStrLst n) in
      (nth 0, nth 1, nth 2, nth 3)
    in
    if (List.length lst <= 0) then ret
    else if (String.length (List.hd lst) < 7) then helper ret (List.tl lst)
    else helper ((getFourNums (split_string (List.hd lst) ' ')) :: ret) (List.tl lst)
  in
  helper [] quads
;;

let getIntersectionList segs =
  let rec helper ret segsLeft allSegs =
    let rec inner ret_inner seg sleft =
      if (List.length sleft <= 0) then ret_inner
      else
	let result = getIntersection seg (List.hd sleft) in
	match result with
	  | Intersection(x,y) -> inner ((x,y) :: ret_inner) seg (List.tl sleft)
	  | _ -> inner ret_inner seg (List.tl sleft)
      in
   if List.length segsLeft <= 0 then ret
   else helper ((inner ((fun (a,b,c,d)->([(a,b);(c,d)])) (List.hd segsLeft)) (List.hd segsLeft) allSegs) @ ret) (List.tl segsLeft) (List.tl allSegs)
  in
  helper [] segs segs
;;

let print_four a b c d = print_endline ("(" ^ (string_of_int a) ^", "^(string_of_int b) ^") <-> ("^(string_of_int c) ^", "^ (string_of_int d)^")");;

let createGraph segList num =
  let rec createVertices tbl isects defLen =
    if (List.length isects > 0) then (Hashtbl.replace tbl (List.hd isects) (Hashtbl.create defLen); createVertices tbl (List.tl isects) defLen)
    else tbl
  in
  let addLink (v1, v2) tbl n =
    let al_helper a b = Hashtbl.replace (Hashtbl.find tbl a) b n in
    ((*(fun (a,b) (c,d) -> print_four a b c d) v1 v2;*)
    al_helper v1 v2; al_helper v2 v1; ())
  in
  let rec addAllLinks edges tbl n =
    let rec over_hops hops =
      if (List.length hops > 0) then (addLink (List.hd hops) tbl n; over_hops (List.tl hops))
      else ()
    in
    let rec over_edges edgeList =
      if (List.length edgeList > 0) then (List.map (fun ((a,b), (c,d)) -> print_four a b c d) ((List.hd edgeList)#createHopsList);print_endline "";over_hops ((List.hd edgeList)#createHopsList); over_edges (List.tl edgeList))
      else ()
    in
    over_edges edges;
    tbl
  in
  let rec createEdgeList ret segs isects =
    if (List.length segs > 0) then (
      let (a,b,c,d) = (List.hd segs) in
      let ed = new edge a b c d in
      ed#findIntersections isects;
      createEdgeList (ed :: ret) (List.tl segs) isects)
   else ret
  in
  let launch isects =
    addAllLinks (createEdgeList [] segList isects) (createVertices (Hashtbl.create 2500) isects 10) num
  in
  let rec postProcess tbl prevLen =
    let spliceOut k =
      let oldval = Hashtbl.find tbl k in
      let link k1 k2 v =
	Hashtbl.remove (Hashtbl.find tbl k1) k;
	Hashtbl.remove (Hashtbl.find tbl k2) k;
	Hashtbl.replace (Hashtbl.find tbl k1) k2 v;
	Hashtbl.replace (Hashtbl.find tbl k2) k1 v;
	()
      in
      let keys = Hashtbl.fold (fun k v prev -> k :: prev) tbl [] in
      link (List.nth keys )0 (List.nth keys 1) oldval
    in
    Hashtbl.fold (fun k v prev ->
      if Hashtbl.length v == 2 then spliceOut k
      else ()) tbl ();
    if (Hashtbl.length tbl - prevLen != 0) then
      postProcess tbl (Hashtbl.length tbl)
    else tbl
  in
  print_int_int_list (getIntersectionList segList);
  postProcess (launch (getIntersectionList segList)) 0
;;

let getPenLifts graph =
  let rec helper ret g =
    let getScore v = Hashtbl.fold (fun k v prev -> prev + v) v 0 in
    let cmp k prev =
      let curr = Hashtbl.find g k in
      match prev with
	| Some(key) -> 
	  if (getScore curr mod 2 != 0) then Some(k) else prev
	| None -> if (Hashtbl.length curr > 0) then Some(k) else None
    in
    let getStart tbl =
      Hashtbl.fold (fun k v prev -> match prev with Some((a,b)) -> cmp k prev | None -> if (Hashtbl.length v > 0) then Some(k) else None) tbl None
    in
    let rec wander key =
      let tbl = Hashtbl.find g key in
      let getFirst =
	Hashtbl.fold (fun k v prev -> match prev with Some((a,b)) -> cmp k prev | None -> Some(k)) tbl None
      in
      let decrementOrRemove k1 k2 =
	let helper a b =
	  let v = Hashtbl.find g a in
	  let num = (Hashtbl.find v b) in
	  Hashtbl.replace v b (num - 1);
	  if (num - 1 <= 0) then Hashtbl.remove v b else ()
	in
	helper k1 k2; helper k2 k1; ()
      in
      match getFirst with
	| Some(k) ->
	  (print_endline ("Moving to " ^ (string_of_int (fst k)) ^ " " ^ (string_of_int (snd k)) ^ "...");
	  decrementOrRemove k key;
	  wander k)
	| None -> print_endline "PEN LIFT!"
    in
    match getStart g with
      | Some(k)-> (print_endline ("Starting with " ^ (string_of_int (fst k)) ^ " " ^ (string_of_int (snd k)) ^ "..."); wander k; helper (ret + 1) g)
      | None -> max (ret - 1) 0
  in
  helper 0 graph
;;

let segs = 
"{\"0 0 1 0\",   \"2 0 4 0\",   \"5 0 8 0\",   \"9 0 13 0\",
 \"0 1 1 1\",   \"2 1 4 1\",   \"5 1 8 1\",   \"9 1 13 1\",
 \"0 0 0 1\",   \"1 0 1 1\",   \"2 0 2 1\",   \"3 0 3 1\",
 \"4 0 4 1\",   \"5 0 5 1\",   \"6 0 6 1\",   \"7 0 7 1\",
 \"8 0 8 1\",   \"9 0 9 1\",   \"10 0 10 1\", \"11 0 11 1\",
 \"12 0 12 1\", \"13 0 13 1\"}"
;;

(*let e = new edge 0 0 13 0;;*)
(*e#findIntersections (getIntersectionList (parseSegments segs));;
e#printIsectList;;
List.map (fun ((a,b), (c,d)) -> print_four a b c d) e#createHopsList*)
(*print_endline (;;*)
let getNumLifts segString n =
  (getPenLifts (createGraph (parseSegments segString) n))
;;

let getFileContents fname =
  let rec helper ret chan =
    try
      helper ((input_line chan) :: ret) chan
    with End_of_file -> (close_in chan; String.concat "" (List.rev ret))
  in
  helper [] (open_in fname)
;;
print_endline (getFileContents (Sys.argv.(1)));;
print_endline (string_of_int (getNumLifts (getFileContents (Sys.argv.(1))) (int_of_string Sys.argv.(2))));;

