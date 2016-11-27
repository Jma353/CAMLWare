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

  let container = svg <.> g in                 (* Base container *)
  let with_reg = register 50 75 container in   (* Add a register *)

  (* Setup a path *)
  let scale = linear (0,100) (0,100) in
  let d = list_to_coord_js_array [(40.,40.);(70.,10.)] in
  path d scale scale "none" "black" 1 with_reg





;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 300; height = 300 } 50) ()
