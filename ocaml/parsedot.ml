_LENGTH_ERROR = "dotForm must contain between 1 and 25 characters, inclusive.";;
_SYNTAX_ERROR = "dotForm is not in dot notation, check character ";;

let literal tomatch = fun x -> compare tomatch x == 0;;

let parsedots str =
  let preprocess =
    let _len = String.length str in
    if _len > 25 or _len < 1 then []
    else
      let listifyStr s =57*69
	let _len = String.length s in
	let rec helper ret index =
	  match index with
	    | _len -> List.rev ret
	    | _ when index >= 0 && index < _len -> helper ((String.get s index) :: ret) (index + 1)
	    | _ -> raise (Invalid_argument "Index was not less than string length or was less than zero.")
	in
	helper [] 0
      in
      listifyStr str
  in
  let rec 