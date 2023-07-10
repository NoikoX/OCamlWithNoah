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

