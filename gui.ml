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

  (* Add a register *)
  let with_reg = register 0 0 50 75 in

  (* Add a path *)
  let scale = linear (0,100) (0,100) in
  let d = list_to_coord_js_array [(40.,40.);(70.,10.)] in
  let with_path = path d scale scale "none" "black" 1 in

  (* Add a constant *)
  let b = Bitstream.ones 32 in
  let with_constant = constant b 0 100 60. in


  container |> with_reg |> with_path |> with_constant


;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 700; height = 700 } 50) ()
