let rec hd = function
| [] -> failwith "invalid"
| h::_ -> h;;
let rec tl = function
| [] -> failwith "invalid"
| _::t -> t;;
let rec length = function
| [] -> 0
| h::t -> 1 + length t;;
let rec append l1 l2 = match l1 with 
| [] -> l2
| h::t -> h :: append t l2;;
let rec rev = function 
| [] -> []
| h::t -> rev t @ [h];;
let rec nth l n = match l with
| [] -> failwith "invalid"
| h::t -> if n = 0 then h else nth t (n-1);;`