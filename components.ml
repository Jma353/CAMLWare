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

let border_radius tl tr br bl =
  (px tl) ^ " " ^ (px tr) ^ " " ^ (px br) ^ " " ^ (px bl)

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
  let circ =
    (append "circle"
    |. flt attr "cx" cx
    |. flt attr "cy" cy
    |. flt attr "r" r
    |. str style "fill" "white"
    |. str style "stroke" "black"
    |. int style "stroke-width" 1) in
  svg |- circ

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

(* Constant Component *)
let constant b (x:float) (y:float) (edge:float) svg =
  let hex_str = Bitstream.bitstream_to_hexstring b in
  let w = edge in
  let h = w *. 0.3 in
  let x_i = i_of_f x in let y_i = i_of_f y in
  let g = append "g" |. str attr "transform" (translate x_i y_i) in
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

(* Arithmetic AND Component *)
let and_c (x:float) (y:float) (edge:float) svg = svg |> and_helper x y edge

(* Arithmetic NAND Component *)
let nand_c (x:float) (y:float) (edge:float) svg =
  svg |> and_c x y edge |> neg_dot x y edge

(* Arithmetic OR Component *)
let or_c (x:float) (y:float) (edge:float) svg = svg |> or_helper x y edge

(* Arithmetic NOR Component *)
let nor_c (x:float) (y:float) (edge:float) svg =
  svg |> or_c x y edge |> neg_dot x y edge

(* XOR Component *)
let xor_c (x:float) (y:float) (edge:float) svg = svg |> xor_helper x y edge

(* NXOR Component *)
let nxor_c (x:float) (y:float) (edge:float) svg =
  svg |> xor_c x y edge |> neg_dot x y edge
