(*Write a function named gcd that takes 2 numbers and computes their greatest common divisor*)
let rec gcd a b = 
  match a, b with 
 | _, 0 -> a
 | 0, _ -> b
 | _ -> gcd b (a mod b) 
