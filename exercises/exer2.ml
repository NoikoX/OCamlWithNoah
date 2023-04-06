(*Write a function named power that takes in a power 'n' and a float 'x' and returns x ^ n*)
let rec power (x:float) (n:float) : float = 
  if n = 0. then 1.0 
  else  x *. power x (n-.1.);;
  
let square (x : float) = power x 2.  

 