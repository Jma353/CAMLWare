open D3
open Extensions
open Components
open View
open Circuit.Formatter

(* Dimensions of the window *)
type dims = { width : int; height: int }

(* View info *)
let view dims padding =

  (* Total SVG Dimensions *)
  let width = dims.width + 2 * padding in
  let height = dims.height + 2 * padding in

  (* Base SVG *)
  let svg =
    (append "svg"
    |. int attr "width" width
    |. int attr "height" height) in

  (* Circuit Container *)
  let g =
    (append "g"
    |. str attr "transform" (translate padding padding)) in

  (* Border *)
  let border_rect =
    (append "rect"
    |. int attr "x" 0
    |. int attr "y" 0
    |. int attr "width" width
    |. int attr "height" height
    |. str style "stroke" "black"
    |. str style "fill" "none"
    |. int style "stroke-width" 1) in

  (* Base Container *)
  let bordered_svg = (svg |- border_rect) in

  (* Add things to g *)


  (* Return the result *)
  bordered_svg |- g


;;

let _ =
  (run ~node:(Dom_html.document##body)
  (view {
    width = 800;
    height = 500;
  } 100) ())
