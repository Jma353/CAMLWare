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

(* Constant Component *)
let constant b x y w svg =
  let hex_str = Bitstream.bitstream_to_hexstring b in
  let h = w *. 0.3 in
  let g = append "g" |. str attr "transform" (translate x y) in
  let frame =
    (append "rect"
      |. flt attr "width" w
      |. flt attr "height" h
      |. int attr "x" 0
      |. int attr "y" 0
      |. str style "stroke" "black"
      |. str style "fill" "transparent"
      |. int style "stroke-width" 1) in
  let words =
    (append "text"
      |. flt attr "x" (w *. 0.5)
      |. flt attr "y" (h *. 0.5)
      |. str attr "text-anchor" "middle"
      |. str attr "alignment-baseline" "middle"
      |. str attr "font-family" "Courier"
      |. str attr "font-size" ((string_of_float (w /. 7.5)) ^ "px")
      |. str attr "fill" "black"
      |. text (fun _ _ _ -> hex_str)) in
  let gnode = (g |- frame) |- words in
  svg |- gnode

(* Register Component *)
let register x y w h svg =
  let frame =
    (append "rect"
    |. int attr "width" w
    |. int attr "height" h
    |. int attr "x" x
    |. int attr "y" y
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
