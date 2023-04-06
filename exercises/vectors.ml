type vector3 = {x:float; y:float; z:float}

let v1 = {x=1.; y=1.; z=1.}
let v2 = {x=2.; y=2.; z=2.}
let v3 = {x=3.; y=3.; z=3.}

let vector3_to_string v =
  Printf.sprintf "(%g, %g, %g)" v.x v.y v.z;;

let vector3_add (v1:vector3) (v2:vector3) : vector3 =
  { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

let mangitude v = ((v.x)**2. +. (v.y)**2. +. (v.z)**2.)**0.5;;

let vector3_max v1 v2 = if mangitude v1 > mangitude v2 then v1 else v2;;


let result = vector3_add v1 (vector3_max v2 v3)



