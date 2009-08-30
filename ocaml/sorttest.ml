let thelist = [1;2;4;8;16;32;64;128;256;512;1024;];;
(*let thelist = [6;5;4;3;2;1];;*)
let thelist = [43;243;3;432;3;45;432;435;43;53;45;23;32;6;657;768;43;657;5;436;43;3;6;];;
let thestrings = ["abcde"; "a"; "abcd"; "abc"; "abcdefg";];;

print_endline (Jdh.print_list (Jdh.intsort thelist) string_of_int ", ");;
print_endline (Jdh.print_list (Jdh.sort (fun a b -> (String.length a) - (String.length b)) thestrings ) (fun a -> a) ", ");;