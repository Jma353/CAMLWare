open D3
open Extensions
open Components
open View
open Circuit.Formatter

(* Dimensions of the window *)
type dims =
  { width : int; height : int }


(* View info *)
let view dims padding =
  (* Base SVG *)
  let svg =
    (append "svg"
    |. int attr "width"  dims.width
    |. int attr "height" dims.height) in
  (* Global container *)
  let g =
    (append "g"
    |. str attr "transform" (translate padding padding)) in

  (* Border *)
  let border_rect =
    (append "rect"
    |. int attr "x" 0
    |. int attr "y" 0
    |. int attr "width" dims.width
    |. int attr "height" dims.height
    |. str style "stroke" "black"
    |. str style "fill" "none"
    |. int style "stroke-width" 1) in

  (* Base container *)
  let container = (svg |- border_rect) <.> g in

  container

;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 1000; height = 700 } 150) ()
