open Extensions.D3Extended
open Utils
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
  let container = svg <.> g in
  (* Add a register *)
  register 50 75 container
;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 300; height = 300 } 50) ()
