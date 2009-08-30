type graph_node = PricedNode of priced | UnpricedNode of unpriced and
  unpriced = {u_children: graph_node list; u_edgecost: int;} and
  priced = {children: graph_node list; best: int; worst: int; edgecost: int;};;

let bestPrice best worst =
  let sum = List.fold_left (fun x y -> x + y) 0 worst in
  let rec inner ret bst wst =
    if bst == [] then ret else
    let b = List.hd bst in
    let w = List.hd wst in
    let curr = sum - w + b in
    let getRet =
      match ret with
	| None -> Some curr
	| Some r -> Some (min r curr)
    in
    inner getRet (List.tl bst) (List.tl wst)
  in
  match (inner None best worst) with
    | None -> raise (Failure "Did not find best child.")
    | Some s -> (s, sum)
;;
let getFromChildren f children =
  let helper ele =
    match (ele) with
      | PricedNode {best=b;worst=w;} -> f b w
      | _ -> raise (Failure "Must get best or worst from PricedNode.")
  in
  List.map helper children
;;
let lowestToPriced n =
  PricedNode{	children=n.u_children;
		best=n.u_edgecost;
		worst=2*n.u_edgecost;
		edgecost=n.u_edgecost
	    }
;;
let priceNode node =
  match node with
    | {u_children=c; u_edgecost=e;} ->
      let getBest = getFromChildren (fun b w -> b) c in
      let getWorst = getFromChildren (fun b w -> w) c in
      let b,w = bestPrice getBest getWorst in
      PricedNode {children=c; best=b + e; worst=w + 2*e; edgecost=e;}
;;

let rec descendGraph node =
  if node.u_children == [] then lowestToPriced node
  else
    let unpricedList =
      let helper ret c =
	match c with
	  | UnpricedNode un -> (descendGraph un) :: ret
	  | PricedNode _ -> raise (Failure "Cannot descend on priced nodes.")
      in
      List.fold_left helper [] node.u_children
    in
    let toprice = {u_children = unpricedList; u_edgecost=node.u_edgecost;} in
    priceNode toprice
;;

let graphFromLists source dest cost =
  let size = (List.hd (List.sort (fun a b -> b - a) dest)) in
  let edgeArray = Array.init size (fun x -> Array.make size None) in
  let rec helper s d c =
    if s == [] then () else
    let one,two,three = ((List.hd s), (List.hd d), (List.hd c)) in
    edgeArray.(one).(two - 1) <- Some three;
    helper (List.tl s) (List.tl d) (List.tl c)
  in
  helper source dest cost;
  
  let rec down row cst =
    let rec makeAdjacent subret col =
      if col = size then subret else
      match edgeArray.(row).(col) with
	| None -> makeAdjacent subret (col + 1)
	| Some c -> makeAdjacent ((c, col + 1) :: subret) (col + 1)
    in
    let bottom = UnpricedNode {u_children=[]; u_edgecost = cst;} in
    if row = size then bottom
    else
      let adj = makeAdjacent [] 0 in
      if adj == [] then bottom
      else UnpricedNode{u_children = (List.map (fun n -> down (snd n) (fst n)) adj); u_edgecost=cst;}
  in
  down 0 0
;;

let s1 = [0;0;0;1;4;4;6;7;7;7;20];;
let d1 = [1;3;4;2;5;6;7;20;9;10;31];;
let c1 = [10;10;100;10;5;1;1;100;1;1;5];;

let graph = graphFromLists s1 d1 c1;;
let descendable = match graph with
  | UnpricedNode {u_children=c;u_edgecost=e} -> {u_children=c;u_edgecost=e}
  | _ -> raise (Failure "graph should be unpriced to start.")
;;
let getBestFromPriced node =
   match node with
    | PricedNode {best=b;} -> b
    | _ -> raise (Failure "expected something priced.")
;;
print_endline (string_of_int (getBestFromPriced (descendGraph descendable)))