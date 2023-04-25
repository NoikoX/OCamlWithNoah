let rec eval_poly x = function
  | [] -> 0.
  |c :: rest ->c +. x *. (eval_poly x rest);;

let rec derive_poly = function
  | [] -> []
  | [_] -> []
  |c :: rest -> (float_of_int (List.length rest + 1)) *.c :: derive_poly rest;;

  