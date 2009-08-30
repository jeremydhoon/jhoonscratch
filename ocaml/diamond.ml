let rec printLine width j spacing lineList =
  if j = width then (List.rev ("\n" :: lineList))
  else let toPrint = if (j < spacing) then " " else "* " in
	    printLine width (j + 1) spacing (toPrint :: lineList)
;;

let rec printDiamond_h width i dmdLst =
    let doubleWidth = 2*width in
    if i = doubleWidth then dmdLst
    else let spacing = abs(i - width) in
	      printDiamond_h width (i + 1) (dmdLst @ (printLine width 0 spacing []))
;;

let printDiamond width = printDiamond_h width 0 [];;

print_string (String.concat "" (printDiamond 10));;