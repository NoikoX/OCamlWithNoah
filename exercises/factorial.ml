let rec factorial n = if n <=1 then 1 else n * factorial(n-1);;

let rec fac n  = 
  let rec factorial_helper n result = if n = 0 then result else factorial_helper (n-1)(result*n) in factorial_helper n 1;;
  