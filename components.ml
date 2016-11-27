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

(* [px x] expresses a float metric in terms of pixels *)
let px (x:float) = ((string_of_int (int_of_float x)) ^ "px")

(* [i_of_f x] int_of_float alias *)
let i_of_f x = int_of_float x

(* [container x y] assists in the creation of a <g> container, given
 * two float coordinates corresponding to the top left corner *)
let container (x: float) (y: float) =
  let x_i = i_of_f x in let y_i = i_of_f y in
  append "g" |. str attr "transform" (translate x_i y_i)

(* [txt_c x y font_size sym] assists in the creation of text SVG component *)
let txt_c x y font_size sym =
  append "text"
  |. flt attr "x" x
  |. flt attr "y" y
  |. str attr "text-anchor" "middle"
  |. str attr "alignment-baseline" "middle"
  |. str attr "font-family" "Courier"
  |. str attr "font-size" ((string_of_float font_size) ^ "px")
  |. str attr "fill" "black"
  |. text (fun _ _ _ -> sym)

(* [circ_c cx cy r] assists in the creation of a white circle SVG component *)
let circ_c cx cy r =
  append "circle"
  |. flt attr "cx" cx
  |. flt attr "cy" cy
  |. flt attr "r" r
  |. str style "fill" "white"
  |. str style "stroke" "black"
  |. int style "stroke-width" 1

(* Path Component *)
let path d x_scale y_scale fill stroke width interp svg =
  let line_fun = line x_scale y_scale interp in
  let path_comp =
    (append "path"
    |. str attr "d" (use_line line_fun d)
    |. str style "fill" fill
    |. str style "stroke" stroke
    |. int style "stroke-width" width) in
  svg |- path_comp

(* [neg_dot x y edge svg] assists in the creation of a negation dot at the end
 * of a particular logic gate *)
let neg_dot (x:float) (y:float) (edge:float) svg =
  let r = 0.07 *. edge in
  let cx = (x +. edge *. 0.83 +. r) in
  let cy = (y +. 0.5 *. edge) in
  let circ = circ_c cx cy r in
  svg |- circ

(* [not_helper] x y edge svg] assists in the creation of the shape of a NOT
* gate *)
let not_helper (x:float) (y:float) (edge:float) svg =
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let tl = (x +. edge *. 0.2, y +. edge *. 0.2) in
  let mr = (x +. edge *. 0.7, y +. edge *. 0.5) in
  let bl = (x +. edge *. 0.2, y +. edge *. 0.8) in
  let d = list_to_coord_js_array [tl;mr;bl;tl] in
  let my_path = path d scale scale "none" "black" 1 "linear" in
  let r = 0.1 *. edge in
  let cx = (x +. edge *. 0.7 +. r) in
  let cy = (y +. 0.5 *. edge) in
  let circ = circ_c cx cy r in
  (svg |> my_path) |- circ

(* [and_helper x y edge svg] assists in the creation of the shape of an AND
* gate *)
let and_helper (x:float) (y:float) (edge:float) svg =
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let tl = (x, y +. 0.1 *. edge) in
  let tm = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
  let mr = (x +. edge, y +. 0.5 *. edge) in
  let bm = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
  let bl = (x, y +. 0.9 *. edge) in
  let d_1 = list_to_coord_js_array [tl;tm;mr;bm;bl] in
  let path_1 = path d_1 scale scale "none" "black" 1 "basis" in
  let d_2 = list_to_coord_js_array [tl;bl] in
  let path_2 = path d_2 scale scale "none" "black" 1 "linear" in
  svg |> path_1 |> path_2

(* [or_helper x y edge svg] assists in the creation of the shape of an OR
 * gate *)
let or_helper (x:float) (y:float) (edge:float) svg =
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let tl = (x, y +. 0.1 *. edge) in
  let tm = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
  let mr = (x +. edge, y +. 0.5 *. edge) in
  let bm = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
  let bl = (x, y +. 0.9 *. edge) in
  let mm = (x +. 0.2 *. edge, y +. 0.5 *. edge) in
  let d_1 = list_to_coord_js_array [tl;tm;mr;bm;bl] in
  let path_1 = path d_1 scale scale "none" "black" 1 "basis" in
  let d_2 = list_to_coord_js_array [tl;mm;bl] in
  let path_2 = path d_2 scale scale "none" "black" 1 "basis" in
  svg |> path_1 |> path_2

(* [xor_helper x y edge svg] assists in the creation of the shape of an XOR
 * gate *)
let xor_helper (x:float) (y:float) (edge:float) svg =
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let tl = (x +. 0.1 *. edge, y +. 0.1 *. edge) in
  let tm = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
  let mr = (x +. edge, y +. 0.5 *. edge) in
  let bm = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
  let bl = (x +. 0.1 *. edge, y +. 0.9 *. edge) in
  let mm = (x +. 0.2 *. edge, y +. 0.5 *. edge) in
  let d_1 = list_to_coord_js_array [tl;tm;mr;bm;bl] in
  let path_1 = path d_1 scale scale "none" "black" 1 "basis" in
  let d_2 = list_to_coord_js_array [tl;mm;bl] in
  let path_2 = path d_2 scale scale "none" "black" 1 "basis" in
  let xor_tl = (x, y +. 0.1 *. edge) in
  let xor_mm = (x +. 0.1 *. edge, y +. 0.5 *. edge) in
  let xor_bl = (x, y +. 0.9 *. edge) in
  let d_3 = list_to_coord_js_array [xor_tl;xor_mm;xor_bl] in
  let path_3 = path d_3 scale scale "none" "black" 1 "basis" in
  svg |> path_1 |> path_2 |> path_3

(* [box_with_symbol x y edge sym svg] assists in the creation of a box with
 * a text symbol inside of it *)
let box_with_symbol (x:float) (y:float) (edge:float) sym svg =
  let g = container x y in
  let frame =
    (append "rect"
    |. flt attr "width" edge
    |. flt attr "height" edge
    |. int attr "x" 0
    |. int attr "y" 0
    |. int attr "rx" 6
    |. int attr "ry" 6
    |. str style "stroke" "black"
    |. str style "fill" "none"
    |. int style "stroke-width" 1) in
  let sym_c = txt_c (edge *. 0.5) (edge *. 0.5) (edge /. 2.) sym in
  let gnode = (g |- frame) |- sym_c in
  svg |- gnode

(* Constant Component *)
let constant b (x:float) (y:float) (edge:float) svg =
  let hex_str = Bitstream.bitstream_to_hexstring b in
  let w = edge in
  let h = w *. 0.3 in
  let g = container x y in
  let frame =
    (append "rect"
    |. flt attr "width" w
    |. flt attr "height" h
    |. flt attr "x" 0.
    |. flt attr "y" (edge *. 0.5 -. h *. 0.5)
    |. int attr "rx" 6
    |. int attr "ry" 6
    |. str style "stroke" "black"
    |. str style "fill" "none"
    |. int style "stroke-width" 1) in
  let words = txt_c (w *. 0.5) (edge *. 0.5) (w /. 7.5) hex_str in
  let gnode = (g |- frame) |- words in
  svg |- gnode

(* Register Component *)
let register (x:float) (y:float) (edge:float) svg =
  let frame =
    (append "rect"
    |. int attr "width" (i_of_f edge)
    |. int attr "height" (i_of_f edge)
    |. flt attr "x" x
    |. flt attr "y" y
    |. str style "stroke" "black"
    |. str style "fill" "transparent"
    |. int style "stroke-width" 1) in
  let line1 = (line_comp 0
    (int_of_float (0.75 *. edge))
    (int_of_float (0.15 *. edge))
    (int_of_float (0.80 *. edge)) 1 "black") in
  let line2 = (line_comp
    (int_of_float (0.15 *. edge))
    (int_of_float (0.80 *. edge)) 0
    (int_of_float (0.85 *. edge)) 1 "black") in
  ((svg |- frame) |- line1) |- line2

(* Arithmetic NOT Component *)
let arith_not (x:float) (y:float) (edge:float) svg = svg |> not_helper x y edge

(* Arithmetic AND Component *)
let arith_and (x:float) (y:float) (edge:float) svg = svg |> and_helper x y edge

(* Arithmetic NAND Component *)
let arith_nand (x:float) (y:float) (edge:float) svg =
  svg |> arith_and x y edge |> neg_dot x y edge

(* Arithmetic OR Component *)
let arith_or (x:float) (y:float) (edge:float) svg = svg |> or_helper x y edge

(* Arithmetic NOR Component *)
let arith_nor (x:float) (y:float) (edge:float) svg =
  svg |> arith_or x y edge |> neg_dot x y edge

(* Arithmetic XOR Component *)
let arith_xor (x:float) (y:float) (edge:float) svg = svg |> xor_helper x y edge

(* Arithmetic NXOR Component *)
let arith_nxor (x:float) (y:float) (edge:float) svg =
  svg |> arith_xor x y edge |> neg_dot x y edge

(* Logical AND Component *)
let logical_and (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "&&"

(* Logical OR Component *)
let logical_or (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "||"

(* Logical NOT Component *)
let logical_not (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "!"
