open D3
open Extensions

(* [line_comp x1 y1 x2 y2 stroke_width stroke] assists in the creation of a
 * single line. *)
let line_comp x1 y1 x2 y2 stroke_width stroke =
  append "line"
  |. int attr "x1" x1
  |. int attr "y1" y1
  |. int attr "x2" x2
  |. int attr "y2" y2
  |. int style "stroke-width" stroke_width
  |. str style "stroke" stroke


(* Path Component *)
let path d x_scale y_scale fill stroke width svg =
  let line_fun = line x_scale y_scale in
  let path_comp =
    (append "path"
    |. str attr "d" (use_line line_fun d)
    |. str style "fill" fill
    |. str style "stroke" stroke
    |. int style "stroke-width" width) in
  svg |- path_comp


(* Register Component *)
let register w h svg =
  let frame =
    (append "rect"
    |. int attr "width" w
    |. int attr "height" h
    |. int attr "x" 0
    |. int attr "y" 0
    |. str style "stroke" "black"
    |. str style "fill" "transparent"
    |. int style "stroke-width" 1) in
  let line1 = (line_comp 0
    (int_of_float (0.75 *. float_of_int h))
    (int_of_float (0.15 *. float_of_int w))
    (int_of_float (0.80 *. float_of_int h)) 1 "black") in
  let line2 = (line_comp (int_of_float (0.15 *. float_of_int w))
    (int_of_float (0.80 *. float_of_int h)) 0
    (int_of_float (0.85 *. float_of_int h)) 1 "black") in

  ((svg |- frame) |- line1) |- line2
