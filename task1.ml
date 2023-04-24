let rec member comp term lst = match lst with
  | [] -> false
  | h :: t -> if comp term h = 0 then true else member comp term t

let equal_second_components (_, x) (_, y) = compare x y;;

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)

let testing_member () =
  let l =
    [
      __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
      __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
      __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
      __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
      __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The member test succeeds.\n"; [])
  else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)