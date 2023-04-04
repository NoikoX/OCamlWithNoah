type student = {
  first_name:string;
  last_name:string;
  id:int;
  semester:int;
  grades:(int*float) list
}
type database = student list;;


let insert st db = st::db;;

let rec find_by_id id db = 
  match db with [] -> [] | 
  h::t -> if h.id = id then [h] else find_by_id id t;;
let noe = {first_name="Noe";last_name="Lomidze";id=777;semester=1;grades=[(1, 4.0); (2, 3.7); (3, 3.9)];}
let alice = {first_name = "Alice";last_name = "Lee";id = 12345;semester = 2;grades = [(1, 4.0); (2, 3.7); (3, 3.9)];}
let bob = {first_name = "Bob";last_name = "L";id = 67890;semester = 3;grades = [(1, 3.2); (2, 3.6); (3, 3.8)];}
let charlie = {first_name = "Charlie";last_name = "Lee";id = 24680;semester = 1;grades = [(1, 3.8); (2, 3.5); (3, 3.9)];}

let db = insert noe (insert alice [bob;charlie]);;
let stud = find_by_id 777 db;;
let rec find_by_last_name lname db = match db with [] -> [] | h::t -> if h.last_name = lname then h::find_by_last_name lname t else find_by_last_name lname t;;

let rec remove_by_id id db = match db with [] -> [] | h::t -> if h.id <> id then h::remove_by_id id t else remove_by_id id t;;

let rec count_in_semester sem db = match db with [] -> 0 | h::t -> if h.semester = sem then 1 + count_in_semester sem t else count_in_semester sem t;;
