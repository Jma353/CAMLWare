open D3
open Extensions

(* [line_comp x1 y1 x2 y2 stroke_width stroke] assists in the creation of a
 * single line. *)
let line_comp x1 y1 x2 y2 stroke_width stroke =
  append "line"
  |. flt attr "x1" x1
  |. flt attr "y1" y1
  |. flt attr "x2" x2
  |. flt attr "y2" y2
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

(* [circ_c cx cy r w] assists in the creation of a white circle SVG component *)
let circ_c cx cy r w =
  append "circle"
  |. flt attr "cx" cx
  |. flt attr "cy" cy
  |. flt attr "r" r
  |. str style "fill" "white"
  |. str style "stroke" "black"
  |. int style "stroke-width" w

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

(* Wiring from (x1,y2) -> (x2,y2) Component *)
let wiring (x1:float) (y1:float) (x2:float) (y2:float) svg =
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let lst = [(x1,y1);((x1 +. x2) /. 2., y1);((x1 +. x2) /. 2., y2);(x2,y2)] in
  let d = list_to_coord_js_array lst in
  path d scale scale "none" "black" 1 "linear" svg

(* Tunnel helper *)
let tunnel op (l:string) (x:float) (y:float) (edge:float) svg =
  let x1 = op x (edge *. 0.1) in
  let mini_wire = line_comp x1 y x y 1 "black" in
  let label = txt_c (op x1 (edge *. 0.1)) y (edge /. 4.) l in
  svg |- mini_wire |- label

(* Left Tunnel Component *)
let l_tunnel (l:string) (x:float) (y:float) (edge:float) svg =
  tunnel (-.) l x y edge svg

(* Right Tunnel Component *)
let r_tunnel (l:string) (x:float) (y:float) (edge:float) svg =
  tunnel (+.) l x y edge svg

(* [neg_dot x y edge red svg] assists in the creation of a negation dot at the
 * end of a particular logic gate *)
let neg_dot (x:float) (y:float) (edge:float) red svg =
  let r = 0.07 *. edge in
  let cx = (x +. edge *. 0.83 +. r) in
  let cy = (y +. 0.5 *. edge) in
  let circ = circ_c cx cy r (if red then 4 else 1) in
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
  let circ = circ_c cx cy r 1 in
  (svg |> my_path) |- circ

(* [and_helper x y edge red svg] assists in the creation of the shape of an AND
* gate *)
let and_helper (x:float) (y:float) (edge:float) red svg =
  let w = if red then 4 else 1 in
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let tl = (x, y +. 0.1 *. edge) in
  let tm = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
  let mr = (x +. edge, y +. 0.5 *. edge) in
  let bm = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
  let bl = (x, y +. 0.9 *. edge) in
  let d_1 = list_to_coord_js_array [tl;tm;mr;bm;bl] in
  let path_1 = path d_1 scale scale "none" "black" w "basis" in
  let d_2 = list_to_coord_js_array [tl;bl] in
  let path_2 = path d_2 scale scale "none" "black" w "linear" in
  svg |> path_1 |> path_2

(* [or_helper x y edge red svg] assists in the creation of the shape of an OR
 * gate *)
let or_helper (x:float) (y:float) (edge:float) red svg =
  let w = if red then 4 else 1 in
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let tl = (x, y +. 0.1 *. edge) in
  let tm = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
  let mr = (x +. edge, y +. 0.5 *. edge) in
  let bm = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
  let bl = (x, y +. 0.9 *. edge) in
  let mm = (x +. 0.2 *. edge, y +. 0.5 *. edge) in
  let d_1 = list_to_coord_js_array [tl;tm;mr;bm;bl] in
  let path_1 = path d_1 scale scale "none" "black" w "basis" in
  let d_2 = list_to_coord_js_array [tl;mm;bl] in
  let path_2 = path d_2 scale scale "none" "black" w "basis" in
  svg |> path_1 |> path_2

(* [xor_helper x y edge svg] assists in the creation of the shape of an XOR
 * gate *)
let xor_helper (x:float) (y:float) (edge:float) red svg =
  let w = if red then 4 else 1 in
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let tl = (x +. 0.1 *. edge, y +. 0.1 *. edge) in
  let tm = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
  let mr = (x +. edge, y +. 0.5 *. edge) in
  let bm = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
  let bl = (x +. 0.1 *. edge, y +. 0.9 *. edge) in
  let mm = (x +. 0.2 *. edge, y +. 0.5 *. edge) in
  let d_1 = list_to_coord_js_array [tl;tm;mr;bm;bl] in
  let path_1 = path d_1 scale scale "none" "black" w "basis" in
  let d_2 = list_to_coord_js_array [tl;mm;bl] in
  let path_2 = path d_2 scale scale "none" "black" w "basis" in
  let xor_tl = (x, y +. 0.1 *. edge) in
  let xor_mm = (x +. 0.1 *. edge, y +. 0.5 *. edge) in
  let xor_bl = (x, y +. 0.9 *. edge) in
  let d_3 = list_to_coord_js_array [xor_tl;xor_mm;xor_bl] in
  let path_3 = path d_3 scale scale "none" "black" w "basis" in
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
  let gnode = g |- frame |- sym_c in
  svg |- gnode

(* [sub_bits width height x y txt_x txt_y msg svg] assists in the creation of a
 * component that expresses curation of bits (either nth, sub_seq, etc.) *)
let sub_bits edge width height x y txt_x txt_y msg svg =
  let box =
    (append "rect"
    |. flt attr "width"  width
    |. flt attr "height" height
    |. flt attr "x" x
    |. flt attr "y" y
    |. int attr "rx" 2
    |. int attr "ry" 2
    |. str style "stroke" "black"
    |. str style "fill" "none"
    |. int style "stroke-width" 1) in
  let num_bits =
    (txt_c txt_x txt_y
           (edge /. 3.) msg) in
  svg |- box |- num_bits

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
  let gnode = g |- frame |- words in
  svg |- gnode

(* [register_helper b x y edge float svg] assists in the creation of a
 * component that shows a register *)
let register_helper b l (x:float) (y:float) (edge:float) svg =
  let g = container x y in
  let hex_str = Bitstream.bitstream_to_hexstring b in
  let frame =
    (append "rect"
    |. flt attr "width" edge
    |. flt attr "height" edge
    |. flt attr "x" 0.
    |. flt attr "y" 0.
    |. str style "stroke" "black"
    |. str style "fill" "transparent"
    |. int style "stroke-width" 1) in
  let line1 = (line_comp 0.
    (0.75 *. edge)
    (0.15 *. edge)
    (0.80 *. edge) 1 "black") in
  let line2 = (line_comp
    (0.15 *. edge)
    (0.80 *. edge) 0.
    (0.85 *. edge) 1 "black") in
  let bit_vals = txt_c (edge *. 0.5) (edge *. 0.5) (edge /. 7.5) hex_str in
  let label = txt_c (edge *. 0.8) (edge *. 0.2) (edge /. 4.5) l in
  let gnode = g |- frame |- line1 |- line2 |- bit_vals |- label in
  svg |- gnode

(* Rising (UP) Register Component *)
let u_register b l (x:float) (y:float) (edge:float) svg =
  (svg |> register_helper b ("Rising: " ^ l) x y edge)

(* Falling (DOWN) Register Component *)
let d_register b l (x:float) (y:float) (edge:float) svg =
  (svg |> register_helper b ("Falling: " ^ l) x y edge)

(* Input Register Component *)
let i_register b l (x:float) (y:float) (edge:float) svg =
  (svg |> register_helper b ("Input: " ^ l) x y edge)

(* Output Register Component *)
let o_register b l (x:float) (y:float) (edge:float) svg =
  (svg |> register_helper b ("Output: " ^ l) x y edge)

(* MUX2 Component *)
let mux2_c (x:float) (y:float) (edge:float) svg =
  let g = container x y in
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let one   = (0., 0.) in
  let two   = (edge *. 0.6, edge *. 0.2) in
  let three = (edge *. 0.6, edge *. 0.8) in
  let four  = (0., edge) in
  let d = list_to_coord_js_array [one;two;three;four;one] in
  let my_path = path d scale scale "none" "black" 1 "linear" in
  let i0_label = txt_c (edge *. 0.2) (edge *. 0.3) (edge /. 7.5) "i_0" in
  let i1_label = txt_c (edge *. 0.2) (edge *. 0.5) (edge /. 7.5) "i_1" in
  let sel_label = txt_c (edge *. 0.2) (edge *. 0.7) (edge /. 7.5) "sel" in
  let out_wire = (line_comp (edge *. 0.6) (edge /. 2.) edge
    (edge /. 2.) 1 "black") in
  svg |- (((((g |> my_path) |- i0_label) |- i1_label) |- sel_label) |- out_wire)

(* Nth Bit Component *)
let nth_c (x:float) (y:float) (edge:float) (n:int) svg =
  let in_line = (line_comp x (y +. edge /. 2.)
    (0.3 *. edge +. x) (y +. edge /. 2.) 1 "black") in
  let out_line = (line_comp (x +. edge *. 0.7) (y +. edge /. 2.)
    (x +. edge) (y +. edge /. 2.)  1 "black") in
  ((sub_bits edge
    (0.4 *. edge)
    (0.4 *. edge)
    (0.3 *. edge +. x)
    (0.3 *. edge +. y)
    (x +. edge /. 2.)
    (y +. edge *. 0.15)
    (string_of_int n) svg) |- in_line) |- out_line

(* Sebsequence of bits Component *)
let sub_seq_c (x:float) (y:float) (edge:float) (n1: int) (n2: int) svg =
  let in_line = (line_comp x (y +. edge /. 2.)
    (0.15 *. edge +. x) (y +. edge /. 2.) 1 "black") in
  let out_line = (line_comp (x +. edge *. 0.85) (y +. edge /. 2.)
    (x +. edge) (y +. edge /. 2.)  1 "black") in
  ((sub_bits edge
    (0.7 *. edge)
    (0.4 *. edge)
    (0.15 *. edge +. x)
    (0.3 *. edge +. y)
    (x +. edge /. 2.)
    (y +. edge *. 0.15)
    ((string_of_int n1) ^ "-" ^ (string_of_int n2)) svg) |- in_line) |- out_line

(* Arithmetic NOT Component *)
let arith_not (x:float) (y:float) (edge:float) svg =
  svg |> not_helper x y edge

(* Arithmetic AND Component *)
let arith_and (x:float) (y:float) (edge:float) svg =
  svg |> and_helper x y edge false

(* Arithmetic NAND Component *)
let arith_nand (x:float) (y:float) (edge:float) svg =
  svg |> arith_and x y edge |> neg_dot x y edge false

(* Arithmetic OR Component *)
let arith_or (x:float) (y:float) (edge:float) svg =
  svg |> or_helper x y edge false

(* Arithmetic NOR Component *)
let arith_nor (x:float) (y:float) (edge:float) svg =
  svg |> arith_or x y edge |> neg_dot x y edge false

(* Arithmetic XOR Component *)
let arith_xor (x:float) (y:float) (edge:float) svg =
  svg |> xor_helper x y edge false

(* Arithmetic NXOR Component *)
let arith_nxor (x:float) (y:float) (edge:float) svg =
  svg |> arith_xor x y edge |> neg_dot x y edge false

(* Reduction AND Component *)
let red_and (x:float) (y:float) (edge:float) svg =
  svg |> and_helper x y edge true

(* Reduction NAND Component *)
let red_nand (x:float) (y:float) (edge:float) svg =
  svg |> red_and x y edge |> neg_dot x y edge true

(* Reduction OR Component *)
let red_or (x:float) (y:float) (edge:float) svg =
  svg |> or_helper x y edge true

(* Reduction NOR Component *)
let red_nor (x:float) (y:float) (edge:float) svg =
  svg |> red_or x y edge |> neg_dot x y edge true

(* Reduction XOR Component *)
let red_xor (x:float) (y:float) (edge:float) svg =
  svg |> xor_helper x y edge true

(* Reduction NXOR Component *)
let red_nxor (x:float) (y:float) (edge:float) svg =
  svg |> red_xor x y edge |> neg_dot x y edge true

(* Logical AND Component *)
let logical_and (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "&&"

(* Logical OR Component *)
let logical_or (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "||"

(* Logical NOT Component *)
let logical_not (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "!"

(* Bitwise NOT Component *)
let bitwise_not (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "!(b)"

(* < Component *)
let less_than (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "<"

(* > Component *)
let greater_than (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge ">"

(* = Component *)
let equal_to (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "="

(* <= Component *)
let less_than_or_equal_to (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "<="

(* >= Component *)
let greater_than_or_equal_to (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge ">="

(* != Component *)
let not_equal_to (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "!="

(* Add Component *)
let add_c (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "+"

(* Subtract Component *)
let subtract_c (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "-"

(* Shift Left Logical Component *)
let shift_left_logical (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "<<"

(* Shift Right Logical Component *)
let shift_right_logical (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge ">>"

(* Shift Right Arithmetic Component *)
let shift_right_arithmetic (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge ">>>"

(* Concat Component *)
let concat_c (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge "Concat"

(* Let-statement Component *)
let let_c v (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge v

(* Sub-circuit Component *)
let sub_circ_c v (x:float) (y:float) (edge:float) svg =
  svg |> box_with_symbol x y edge v
