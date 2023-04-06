(*Same gcd program but kinda verbose*)
let rec gcd (x:int) (y:int) : int =
  let min = if x < y then x else y
in
let rec dec m = 
  if x mod m = 0 && y mod m = 0
    then m 
else dec (m-1)
in dec min