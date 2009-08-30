let print_list lst stringify sep = 
  let splice accum add =
    if String.length accum > 0 then accum ^ sep ^ add
    else add
  in
  let rec helper ret remaining =
    if remaining = [] then ret
    else
      helper (splice ret (stringify (List.hd remaining))) (List.tl remaining)
  in
  helper "" lst
;;

let int_of_bool b =
  match b with
    | true -> 1
    | false -> 0
;;

let rec fwd n lst = 
  if n == 0 then lst
  else fwd (n - 1) (List.tl)
;;

(* BEGIN MERGESORT FUNCTIONS *)
let rec merge cmp ret left right =
  if left == [] then (*ret @ right*) List.rev ((List.rev right) @ ret)
  else if right == [] then (*ret @ left*) List.rev ((List.rev left) @ ret)
  else (
    let lhd, rhd = List.hd left, List.hd right in
    if cmp lhd rhd < 0 then 
      merge cmp (lhd :: ret) (List.tl left) right
    else
      merge cmp (rhd :: ret) left (List.tl right))
;;

type 'a node_t = {left: 'a branch_t; right: 'a branch_t} and
 'a branch_t = Node of 'a node_t | Leaf of 'a option;;



let treeify thelist =
  let glomp2 lst = Node ({left=Leaf (Some [(List.hd lst);]); right=Leaf (Some [(List.hd (List.tl lst));]);}) in
  let glomp1 lst = Node ({left= Leaf (Some [(List.hd lst);]); right= Leaf None;}) in
  let rec makeBottom f1 f2 ret lst innerSize =
    match innerSize with
      | 0 -> ret
      | 1 -> (f1 lst) :: ret
      | _ -> makeBottom f1 f2 ((f2 lst) :: ret) (List.tl (List.tl lst)) (innerSize - 2)
  in
  let join2 lst = Node ({left=List.hd lst; right = List.hd (List.tl lst);}) in
  let join1 lst = Node ({left=List.hd lst; right = Leaf None;}) in
  let rec helper lvl =
    if List.tl lvl == [] then List.hd lvl
    else helper (makeBottom join1 join2 [] lvl (List.length lvl))
  in
  helper (makeBottom glomp1 glomp2 [] thelist (List.length thelist))
;;

let rec dfsort tree cmp =
  match tree with
    | Node inner -> (
      match inner with
	| {left=Leaf (Some lst); right = Leaf None;} -> (assert ((List.length lst) == 1); lst)
	| {left = Node n; right=Leaf None} -> dfsort (Node n) cmp
	| {left=Leaf (Some llst); right = Leaf (Some rlst)} -> merge cmp [] llst rlst
	| {left=Node ln; right=Node rn;} -> merge cmp [] (dfsort (Node ln) cmp) (dfsort (Node rn) cmp)
	| _ -> raise (Invalid_argument "This combination should not exist."))
    | Leaf _ -> raise (Invalid_argument "Please don't start me off with a leaf.")
;;

let intsort lst = dfsort (treeify lst) (fun a b -> a - b);;
let sort cmp lst = dfsort (treeify lst) cmp;;
(* END MERGESORT *)