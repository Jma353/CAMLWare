open D3
open Extensions
open Components
open Circuit

(* Dimensions of the window *)
type dims =
  { width : int; height : int }


(* View info *)
let view dims padding =
  (* Base SVG *)
  let svg =
    append "svg"
    |. int attr "width"  dims.width
    |. int attr "height" dims.height
  in
  let g =
    append "g"
    |. str attr "transform" (translate padding padding)
  in

  (* Base container *)
  let container = svg <.> g in

  let one = arith_nxor 0. 50. 60. in
  let two = arith_nxor 0. 150. 40. in
  let three = arith_nxor 0. 250. 100. in
  let logic_and = logical_and 0. 350. 60. in
  let logic_or = logical_or 0. 450. 40. in
  let nottt = arith_not 0. 550. 60. in

  (* Construct our view *)
  (container
    |> one
    |> two
    |> three
    |> logic_and
    |> logic_or
    |> nottt)

;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 700; height = 700 } 50) ()
