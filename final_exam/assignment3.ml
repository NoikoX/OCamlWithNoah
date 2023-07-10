type door = Open of int | Closed of int

let toggle = function
| Closed n -> Open n
| Open n -> Closed n

let generate_doors n = 
  let rec helper acc i = if (i-1) = n then List.rev acc
  else helper ((Closed i)::acc) (i + 1) in helper [] 1;;

let pass n d = let rec helper acc i d = match d with
  | [] -> List.rev acc
  | h::t -> if i = n then helper (toggle h :: acc) 1 t
  else helper (h::acc) (i + 1) t in helper [] 1 d;; 

let rec passes n d = if n >= 1 then passes (n-1) (pass n d) else d

let is_perfect_square n =
  let rec helper i =
    if i > float_of_int n then false
    else if i ** 2. = float_of_int n then true
    else helper (i +. 1.)
  in helper 0.

let smart_passing d = 
  let rec helper d i acc = match d with
  | [] -> List.rev acc
  | h::t -> if is_perfect_square i then helper t (i + 1) ((toggle h)::acc) else helper t (i + 1)(h :: acc) in helper d 1 []
  

