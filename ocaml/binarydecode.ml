type decode_result = Failed | Found of int * int

let decode enc left mustBe =
  let nextVal = (enc - left) - mustBe in
  if nextVal >= 0 then Found(mustBe, nextVal)
  else Failed
;;

let rec decodeStringH encoded decoded result =
  if (List.length encoded <= 0) then
    match result with
      | Failed -> None
      | Found(mustBe, nextVal) when nextVal == 0 -> Some (List.rev (mustBe::decoded))
      | _ -> None
  else
    match result with
      | Failed -> None
      | Found(mustBe, nextVal) -> decodeStringH (List.tl encoded) (mustBe :: decoded) (decode (List.hd encoded) mustBe nextVal)
;;

let rec listifyIntH n l =
  if (n <= 0) then List.rev l
  else 
    let modTen = (n mod 10) in
    listifyIntH (n/10) (modTen :: l)
;;
let listifyInt n =
  listifyIntH n []
;;  

let rec makeStringResultsH l res =
  if (List.length l <= 0) then String.concat "" (List.rev res)
  else makeStringResultsH (List.tl l) ((string_of_int (List.hd l)) :: res)

let makeStringResults l =
  match l with
    | Some lst -> makeStringResultsH lst []
    | None -> ""
;;

let rec listifyStringH str lst index =
  if index == String.length str then List.rev lst
  else listifyStringH str (((int_of_char str.[index]) - 48) :: lst) (index + 1)
;;

let listifyString str =
  listifyStringH str [] 0;;

let decodeFormatted input converter =
  let encList = converter input in
  let forBit bit = makeStringResults (decodeStringH (List.tl encList) [] (decode (List.hd encList) 0 bit)) in
  (forBit 0, forBit 1)  

let decodeInt encoded = 
  decodeFormatted encoded listifyInt
;;
let decodeString encoded =
  decodeFormatted encoded listifyString;;

let stringifyDecoded pair =
  "(" ^ (fst pair) ^ ") (" ^ (snd pair) ^ ")";;

print_endline (stringifyDecoded (decodeString (Sys.argv.(1))));;