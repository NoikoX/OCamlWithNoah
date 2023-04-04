let rec length l = match l with [] -> 0 | x::xs -> 1 + length xs;;

let power x n = match x with x -> x ** n;;