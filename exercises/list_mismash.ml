(*function takes three lists and acts like that
[1; 2; 3][4; 5; 6][7; 8; 9] -> [1; 4; 7; 2; 5; 8; 3; 6; 9]   
*)
let rec interleave l1 l2 l3 = if l1 = [] && l2 = [] && l3 = [] then [] else
  match l1 with [] -> interleave l2 l3 l1 |
  h::t -> h::interleave l2 l3 t;;