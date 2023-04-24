let rec count_occurrences list1 =
  let rec update_assoc_list elem count assoc_list =
    match assoc_list with
    | [] -> [(elem, count)]
    | (e, n)::xs -> if elem = e then (e, count+n)::xs else (e, n)::(update_assoc_list elem count xs)
  in
  let rec build_assoc_list list1 assoc_list =
    match list1 with
    | [] -> List.sort (fun (_,n1) (_,n2) -> compare n2 n1) assoc_list
    | x::xs -> build_assoc_list xs (update_assoc_list x 1 assoc_list)
  in
  build_assoc_list list1 [];;

  let testing_count_occurrences () =
    let l =
      [
        __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
        __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
        __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
        __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
    else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
