let rec tail = function
| [] -> None
| [x] -> Some x
| _::t -> tail t;;

(*n th element of the list*)
let rec nth n = function
| [] -> None
| h::t -> if n = 0 then Some h else nth (n-1) t;;

