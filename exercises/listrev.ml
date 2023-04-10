let rev list = let rec r a l =
  match l
  with [] -> a
  | x::xs -> r (x::a) xs
  in r [] list;;
  