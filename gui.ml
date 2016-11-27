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

  let one = nxor_c 0. 50. 60. in
  let two = nxor_c 0. 150. 40. in
  let three = nxor_c 0. 250. 100. in

  (* Construct our view *)
  (container
    |> one
    |> two
    |> three)


;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 700; height = 700 } 50) ()
