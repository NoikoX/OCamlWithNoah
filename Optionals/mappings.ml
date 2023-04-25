let is_empty m =
  m = [];;

let rec get key = function
| [] -> None
| (k,v)::rest -> if k = key then Some v else get key rest;;

let rec put key value = function
| [] -> [(key, value)]
| (k, v)::rest -> if k = key then (key, value)::rest else (k, v)::(put key value rest);;

let rec contains_key key = function
| [] -> false
| (k, _)::rest -> if k = key then true else contains_key key rest;;

let rec remove key = function
| [] -> []
| (k, v)::rest -> if k = key then rest else (k, v)::(remove key rest);;

let rec keys = function
| [] -> []
| (k, _)::rest -> k::(keys rest);;

let rec values = function
| [] -> []
| (_, v)::rest -> v::(values rest);;