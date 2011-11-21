open Constants
open Definitions

(* Vector operations *)

let mag (x,y) =
  sqrt(x*.x +. y*.y)

let normalize (x,y) =
  let m = mag (x,y) in
  if m = 0. then (x,y) else
  (x/.m,y/.m)

let scale (x,y) mag =
  (x*.mag,y*.mag)

let add (x1,y1) (x2,y2) =
  (x1+.x2,y1+.y2)

let dot (x1,y1) (x2,y2) =
  x1*.x2 +. y1*.y2

let slope (x1, y1) (x2, y2) =
  (y2 -. y1) /. (x2 -. x1)

let dist a b = 
  sqrt(dot a b)

let string_of_color c =
  match c with
  Red -> "Red"
  |Blue -> "Blue"

let getx pt =
  let (x,y) = pt in x

let gety pt =
  let (x,y) = pt in y


