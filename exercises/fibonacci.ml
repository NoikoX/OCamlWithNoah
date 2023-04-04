let rec fib n = if n <= 1 then 1 else fib(n-1)+fib(n-2);;
let rec fib2 n = match n with 0 -> 1 | 1 -> 1 | _ -> fib(n-1) + fib(n-2);;
