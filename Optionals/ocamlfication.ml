let rec foo x y b = if x > y then foo y x b (* if true then return average of x and y but ceiled, else average floored*)
  else let avg = float_of_int (x + y) /. 2.0 in
    int_of_float (if b then ceil avg else floor avg)
