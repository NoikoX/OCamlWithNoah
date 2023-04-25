let squaresum l = List.fold_left (+) 0 (List.map (fun x -> x * x) l);;

let float_list l = List.map float l;;

let to_string l = List.fold_left (^) "" (List.map string_of_int l);;

let part_even l = List.filter (fun x -> x mod 2 = 0) l @ List.filter (fun x -> x mod 2 <> 0) l;;

