let rec length l =
  match l with
    [] -> 0
  | h::t -> (length t) + 1;;


let rec contains_zero l =
  match l with
    [] -> false
  | h::t -> if h = 0 then true else contains_zero t;;


let rec odd_elements l = 
  match l with
    [] -> []
  | h1::_::t -> h1::odd_elements t
  | h::_ -> [h];;


let rec take l n =
  if n <= 0 then [] else
    match l with
      [] -> []
    | h::t -> h::take t (n-1);;


let rec drop l n =
  if n <= 0 then l else
    match l with
      [] -> []
    | _::t -> drop t (n-1);;


let rec interleave2 l1 l2 =
  if l1 = [] && l2 = [] then [] else
    match l1 with
      [] -> interleave2 l2 l1
    | h::t -> h::interleave2 l2 t;;
    

let rec interleave3 l1 l2 l3 =
  if l1 = [] && l2 = [] && l3 = [] then [] else
    match l1 with
      [] -> interleave3 l2 l3 l1
    | h::t -> h::interleave3 l2 l3 t;;