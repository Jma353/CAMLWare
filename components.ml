open D3
open Extensions
open Circuit

(* Global Stroke Width *)
let s_w = 2

(* Line Computation
 * Creates a line SVG element from point (x1,y1) -> (x2,y2).  Allows for the
 * specification of the stroke color *)
let line_comp (x1:float) (y1:float) (x2:float) (y2:float) =
  append "line"
  |. flt attr "x1" x1
  |. flt attr "y1" y1
  |. flt attr "x2" x2
  |. flt attr "y2" y2
  |. int style "stroke-width" s_w
  |. str style "stroke" "black"

(* Int of Float Alias
 * An int_of_float alias *)
let i_of_f (x:float) = int_of_float x

(* Float of Int Alias
 * A float_of_int alias *)
let f_of_i (x:int) = float_of_int x

(* String of Int Alias
 * A string_of_int alias *)
let s_of_i (x:int) = string_of_int x

(* Bitstream to Hexstring Alias
 * A bitstream_to_hexstring alias *)
let bs_to_hs b = Bitstream.bitstream_to_hexstring b

(* Container Helper
 * Assists in the creation of a <g> container, given two float coordinates
 * corresponding to the top left corner *)
let container (x:float) (y:float) =
  let x_i = i_of_f x in let y_i = i_of_f y in
  append "g" |. str attr "transform" (translate x_i y_i)

(* Text Element Helper
 * Assists in the creation of a text SVG element.  Allows for the specification
 * of font size, class, and obviously text *)
let text x y font_size clss sym =
  append "text"
  |. flt attr "x" x
  |. flt attr "y" y
  |. str attr "text-anchor" "middle"
  |. str attr "alignment-baseline" "middle"
  |. str attr "font-family" "Courier"
  |. str attr "class" clss
  |. str attr "font-size" ((string_of_float font_size) ^ "px")
  |. str attr "fill" "black"
  |. text (fun _ _ _ -> sym)

(* Circle Element Helper
 * Assists in the creation of a white circle SVG component *)
let circ_c cx cy r =
  append "circle"
  |. flt attr "cx" cx
  |. flt attr "cy" cy
  |. flt attr "r" r
  |. str style "fill" "white"
  |. str style "stroke" "black"
  |. int style "stroke-width" s_w

(* Path Element Helper
 * Assists in the creation of a path according to data and interpolation *)
let path d interp =
  let scale = linear (0,100) (0,100) in (* 1:1 ratio *)
  let line_fun = line scale scale interp in
  (append "path"
  |. str attr "d" (use_line line_fun d)
  |. str style "fill" "none"
  |. str style "stroke" "black"
  |. int style "stroke-width" s_w)

(* Rectangle Element Helper
 * Assists in the creation of a rectangle SVG component *)
let rect_c (w:float) (h:float) (x:float) (y:float) (rx:float) (ry:float) =
  append "rect"
  |. flt attr "width" w
  |. flt attr "height" h
  |. flt attr "x" x
  |. flt attr "y" y
  |. flt attr "rx" rx
  |. flt attr "ry" ry
  |. str style "stroke" "black"
  |. str style "fill" "none"
  |. int style "stroke-width" s_w

(* Box With Symbol Helper
 * Assists in the creation of a box with a singular symbol in it *)
let box_with_symbol (x:float) (y:float) (edge:float) sym svg =
  let g = container x y in
  let frame = rect_c edge edge 0. 0. 1. 1. in
  let sym_c = text (edge *. 0.5) (edge *. 0.5) (edge /. 4.5) "" sym in
  svg |- (g |- frame |- sym_c)

(* Mini-Module with all Wiring Components *)
module Wiring = struct

  (* HELPERS *)

  (* Tunnel Helper
   * Assists in the creation of a tunnel component *)
  let tunnel op (l:string) (x:float) (y:float) (edge:float) anchor svg =
    let x1        = op x (edge *. 0.5) in
    let mini_wire = line_comp x1 y x y in
    let label     = ((text (op x1 (edge *. 0.1)) y (edge /. 4.5) "" l)
                      |. str attr "text-anchor" anchor) in
    svg |- mini_wire |- label

  (* COMPONENTS *)

  (* Wiring Component *)
  let wiring (x1:float) (y1:float) (x2:float) (y2:float) svg =
    let d = _d [(x1,y1);((x1+.x2)/.2.,y1);((x1+.x2)/.2.,y2);(x2,y2)] in
    svg |- (path d "linear")

  (* Left Tunnel Component *)
  let l_tunnel (l:string) (x:float) (y:float) (edge:float) svg =
    tunnel (-.) l x y edge "end" svg

  (* Right Tunnel Component *)
  let r_tunnel (l:string) (x:float) (y:float) (edge:float) svg =
    tunnel (+.) l x y edge "start" svg

end

(* Mini-Module with all Gate Components *)
module Gates = struct

  (* HELPERS *)

  (* Negative Dot Helper
   * Assists in the creation of a negative dot for a logic gate *)
  let neg_dot (x:float) (y:float) (edge:float) =
    let r  = (0.07*.edge) in
    let cx = (x+.edge*.0.83+.r) in
    let cy = (y+.0.5*.edge) in
    circ_c cx cy r

  (* Not Helper
   * Assists in the creation of a NOT gate
   * NOTE: Adds it to an SVG `svg` *)
  let not_helper (x:float) (y:float) (edge:float) svg =
    let tl      = (x, y +. edge *. 0.2) in
    let mr      = (x +. edge *. 0.8, y +. edge *. 0.5) in
    let bl      = (x, y +. edge *. 0.8) in
    let d       = _d [tl;mr;bl;tl] in
    let my_path = path d "linear" in
    let r       = 0.1 *. edge in
    let cx      = (x +. edge *. 0.8 +. r) in
    let cy      = (y +. 0.5 *. edge) in
    let circ    = circ_c cx cy r in
    svg |- my_path |- circ

  (* And Helper
   * Assists in the creation of an AND gate
   * NOTE: Adds it to an SVG `svg` *)
  let and_helper (x:float) (y:float) (edge:float) svg =
    print_endline (string_of_float y);
    let tl     = (x, y) in
    let tm     = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
    let mr     = (x +. edge, y +. 0.5 *. edge) in
    let mm     = (x +. 0.85 *. edge, y +. 0.5 *. edge) in
    let bm     = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
    let bl     = (x, y +. edge) in
    let d_1    = _d [tl;tm;mr;bm;bl] in
    let path_1 = path d_1 "basis" in
    let d_2    = _d [tl;bl] in
    let path_2 = path d_2 "linear" in
    let d_3    = _d [mm;mr] in
    let path_3 = path d_3 "linear" in
    svg |- path_1 |- path_2 |- path_3

  (* Or Helper
   * Assists int he creation of an OR gate
   * NOTE: Adds it to an SVG `svg` *)
  let or_helper (x:float) (y:float) (edge:float)  svg =
    let tl     = (x, y) in
    let tm     = (x +. 0.5 *. edge, y +. 0.1 *. edge) in
    let mr     = (x +. edge, y +. 0.5 *. edge) in
    let bm     = (x +. 0.5 *. edge, y +. 0.9 *. edge) in
    let bl     = (x, y +. edge) in
    let mm     = (x +. 0.2 *. edge, y +. 0.5 *. edge) in
    let mm2    = (x +. 0.85 *. edge, y +. 0.5 *. edge) in
    let d_1    = _d [tl;tm;mr;bm;bl] in
    let path_1 = path d_1 "basis" in
    let d_2    = _d [tl;mm;bl] in
    let path_2 = path d_2 "basis" in
    let d_3    = _d [mm2;mr] in
    let path_3 = path d_3 "linear" in
    svg |- path_1 |- path_2 |- path_3

  (* Xor Helper
   * Assists int he creation of an XOR gate
   * NOTE: Adds it to an SVG `svg` *)
  let xor_helper (x:float) (y:float) (edge:float) svg =
    let tl     = (x +. 0.1 *. edge, y) in
    let tm     = (x +. 0.5 *. edge, y) in
    let mr     = (x +. edge, y +. 0.5 *. edge) in
    let bm     = (x +. 0.5 *. edge, y +. edge) in
    let bl     = (x +. 0.1 *. edge, y +. edge) in
    let mm     = (x +. 0.2 *. edge, y +. 0.5 *. edge) in
    let mm2    = (x +. 0.85 *. edge, y +. 0.5 *. edge) in
    let d_1    = _d [tl;tm;mr;bm;bl] in
    let path_1 = path d_1 "basis" in
    let d_2    = _d [tl;mm;bl] in
    let path_2 = path d_2 "basis" in
    let xor_tl = (x, y +. 0.1 *. edge) in
    let xor_mm = (x +. 0.1 *. edge, y +. 0.5 *. edge) in
    let xor_bl = (x, y +. 0.9 *. edge) in
    let d_3    = _d [xor_tl;xor_mm;xor_bl] in
    let path_3 = path d_3 "basis" in
    let d_4    = _d [mm2;mr] in
    let path_4 = path d_4 "linear" in
    svg |- path_1 |- path_2 |- path_3 |- path_4

  (* Reduce Text Helper
   * Assists in the generation of `RED` text for gates
   * NOTE: Adds it to an SVG `svg` *)
  let red_text (x:float) (y:float) (edge:float) svg =
    svg |- (text (x +. edge /. 2.) (y +. edge /. 2.) (edge /. 6.5) "" "RED")

  (* Reduce Line Helper
   * Assists in the generation of a line to capture input wires into
   * reduction gates
   * NOTE: Adds it to an SVG `svg` *)
  let red_line (x:float) (y:float) (edge:float) svg =
    svg |- (line_comp x (y +. edge *. 0.2) x (y +.edge *. 0.8))

  (* COMPONENTS *)

  (* Arithmetic NOT Component *)
  let bitwise_not (x:float) (y:float) (edge:float) svg =
    svg |> not_helper x y edge

  (* Arithmetic AND Component *)
  let arith_and (x:float) (y:float) (edge:float) svg =
    svg |> and_helper x y edge

  (* Arithmetic NAND Component *)
  let arith_nand (x:float) (y:float) (edge:float) svg =
    svg |> arith_and x y edge |- neg_dot x y edge

  (* Arithmetic OR Component *)
  let arith_or (x:float) (y:float) (edge:float) svg =
    svg |> or_helper x y edge

  (* Arithmetic NOR Component *)
  let arith_nor (x:float) (y:float) (edge:float) svg =
    svg |> arith_or x y edge |- neg_dot x y edge

  (* Arithmetic XOR Component *)
  let arith_xor (x:float) (y:float) (edge:float) svg =
    svg |> xor_helper x y edge

  (* Arithmetic NXOR Component *)
  let arith_nxor (x:float) (y:float) (edge:float) svg =
    svg |> arith_xor x y edge |- neg_dot x y edge

  (* Reduction AND Component *)
  let red_and (x:float) (y:float) (edge:float) svg =
    svg |> and_helper x y edge |> red_text x y edge |> red_line x y edge

  (* Reduction NAND Component *)
  let red_nand (x:float) (y:float) (edge:float) svg =
    svg |> red_and x y edge |- neg_dot x y edge

  (* Reduction OR Component *)
  let red_or (x:float) (y:float) (edge:float) svg =
    svg |> or_helper x y edge |> red_text x y edge |> red_line x y edge

  (* Reduction NOR Component *)
  let red_nor (x:float) (y:float) (edge:float) svg =
    svg |> red_or x y edge |- neg_dot x y edge

  (* Reduction XOR Component *)
  let red_xor (x:float) (y:float) (edge:float) svg =
    svg |> xor_helper x y edge |> red_text x y edge |> red_line x y edge

  (* Reduction NXOR Component *)
  let red_nxor (x:float) (y:float) (edge:float) svg =
    svg |> red_xor x y edge |- neg_dot x y edge

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
  let arith_not (x:float) (y:float) (edge:float) svg =
    svg |> box_with_symbol x y edge "(-)"

end

(* Mini-Module with all SubBit Components *)
module SubBits = struct

  (* HELPERS *)

  (* Sub Bits Helper
   * Assists in the creation of a component that expresses some sort of
   * filtering of bits in a bitstream *)
  let sub_bits edge width height x y txt_x txt_y msg svg =
    let box      = rect_c width height x y 2. 2. in
    let num_bits = text txt_x txt_y (edge /. 3.) "" msg in
    svg |- box |- num_bits

  (* COMPONENTS *)

  (* Nth Bit Component *)
  let nth_c (n:int) (x:float) (y:float) (edge:float) svg =
    let in_line = (line_comp x (y +. edge /. 2.)
           (0.3 *. edge +. x) (y +. edge /. 2.)) in
    let out_line = (line_comp (x +. edge *. 0.7) (y +. edge /. 2.)
                                    (x +. edge) (y +. edge /. 2.)) in
    (sub_bits edge
      (0.4 *. edge)
      (0.4 *. edge)
      (0.3 *. edge +. x)
      (0.3 *. edge +. y)
      (x +. edge /. 2.)
      (y +. edge *. 0.15)
      (string_of_int n) svg) |- in_line |- out_line

  (* Sebsequence of Bits Component *)
  let sub_seq_c (n1: int) (n2: int) (x:float) (y:float) (edge:float) svg =
    let in_line = (line_comp x (y +. edge /. 2.)
          (0.15 *. edge +. x) (y +. edge /. 2.)) in
    let out_line = (line_comp (x +. edge *. 0.85) (y +. edge /. 2.)
                                     (x +. edge) (y +. edge /. 2.)) in
    (sub_bits edge
      (0.7 *. edge)
      (0.4 *. edge)
      (0.15 *. edge +. x)
      (0.3 *. edge +. y)
      (x +. edge /. 2.)
      (y +. edge *. 0.15)
      ((s_of_i n1) ^ "-" ^ (s_of_i n2)) svg) |- in_line |- out_line

end

(* Mini-Module with all Rising and Falling Register Components *)
module Registers = struct

  (* HELPERS *)

  (* Register Helper
   * Assists in the creation of a base register *)
  let base_register_helper b l type_l (x:float) (y:float) (edge:float) =
    let g          = container x y in
    let hex_str    = bs_to_hs b in
    let frame      = rect_c edge edge 0. 0. 2. 2. in
    let bit_vals   = text (edge *. 0.5) (edge *. 0.5) (edge /. 7.5) l hex_str in
    let label      = text (edge *. 0.6) (edge *. 0.2) (edge /. 7.5) "" l in
    let type_label = text (edge *. 0.1) (edge *. 0.2) (edge /. 7.5) "" type_l in
    g |- frame |- bit_vals |- label |- type_label

  (* Register (Non-Input/Output) Helper
   * Assists in the creation of a u/d_register component *)
  let ud_register_helper b l type_l (x:float) (y:float) (edge:float) =
    let b     = base_register_helper b l type_l x y edge in
    let line1 = (line_comp 0.
      (0.75 *. edge) (0.15 *. edge)
      (0.80 *. edge)) in
    let line2 = (line_comp
      (0.15 *. edge) (0.80 *. edge) 0.
      (0.85 *. edge)) in
    b |- line1 |- line2

  (* COMPONENTS *)

  (* Rising (UP) Register Component *)
  let u_register b l (x:float) (y:float) (edge:float) svg =
    svg |- (ud_register_helper b l "R" x y edge)

  (* Falling (DOWN) Register Component *)
  let d_register b l (x:float) (y:float) (edge:float) svg =
    svg |- (ud_register_helper b l "F" x y edge)

  (* Output Register Component *)
  let o_register b l (x:float) (y:float) (edge:float) svg =
    svg |- (base_register_helper b l "O" x y edge)

end

(* Mini-Module with all Comparator Components *)
module Comparators = struct

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

end

(* Mini-Module with all Arithmetic Components *)
module Arithmetics = struct

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

end

(* Mini-Module with all Misc Components *)
module Miscs = struct

  (* Constant Component *)
  let constant b (x:float) (y:float) (edge:float) svg =
    let hex_str = Bitstream.bitstream_to_hexstring b in
    let w       = edge in
    let h       = w *. 0.3 in
    let g       = container x y in
    let frame   = rect_c w h 0. (edge *. 0.5 -. h *. 0.5) 6. 6. in
    let words   = text (w *. 0.5) (edge *. 0.5) (w /. 8.) "" hex_str in
    svg |- (g |- frame |- words)

  (* MUX2 Component *)
  let mux2_c (x:float) (y:float) (edge:float) svg =
    let g         = container x y in
    let one       = (0., 0.) in
    let two       = (edge *. 0.6, edge *. 0.2)  in
    let three     = (edge *. 0.6, edge *. 0.6)  in
    let four      = (0., edge *. 0.8) in
    let d_1       = _d [one;two;three;four;one] in
    let my_path_1 = path d_1 "linear" in
    let one_1     = (edge *. 0.6, edge *. 0.4) in
    let two_1     = (edge *. 0.8, edge *. 0.4) in
    let three_1   = (edge *. 0.8, edge *. 0.5) in
    let four_1    = (edge, edge *. 0.5) in
    let d_2       = _d [one_1;two_1;three_1;four_1] in
    let my_path_2 = path d_2 "linear" in
    let one_2     = (0., edge) in
    let two_2     = (edge *. 0.3, edge) in
    let three_2   = (edge *. 0.3, edge *. 0.7) in
    let d_3       = _d [one_2;two_2;three_2] in
    let my_path_3 = path d_3 "linear" in
    let mux_txt = text (edge *. 0.3) (edge *. 0.4) (edge /. 5.) "" "Mux" in
    svg |- (g |- my_path_1 |- my_path_2 |- my_path_3 |- mux_txt)

  (* Let-statement Component *)
  let let_c v (x:float) (y:float) (edge:float) svg =
    svg |> box_with_symbol x y edge v

  (* Sub-circuit Component *)
  let sub_circ_c v (x:float) (y:float) (edge:float) svg =
    svg |> box_with_symbol x y edge v

  (* Concat Component *)
  let concat_c (x:float) (y:float) (edge:float) svg =
    svg |> box_with_symbol x y edge "Concat"

  (* SVG Container *)
  let svg_container_div svg_elmt =
    static "div"
    |. str attr "class" "svg-house"
    |. seq [svg_elmt]

  (* Initial View Component *)
  let initial_svg width height padding =
    let width = width + 2 * padding in
    let height = height + 2 * padding in
    let svg =
      (append "svg"
      |. int attr "width" width
      |. int attr "height" height
      |. str style "float" "left") in
    let g =
      (append "g"
      |. str attr "class" "circuit"
      |. str attr "transform" (translate padding padding)) in
    let border_rect =
      rect_c (f_of_i (width-4)) (f_of_i (height-4)) 2. 2. 4. 4. in
    let bordered_svg = (svg |- border_rect) in
    let circuit = bordered_svg <.> g in
    circuit

  (* Clock Component *)
  let clock () =
    static "div"
    |. str attr "class" "clock"
    |. html (fun _ _ _ -> "Clock: 0")

end

(* Mini-Module with all view Components that Trigger View Changes *)
module Triggers = struct

  (* Input Register
   * NOTE: This register takes in a function that accepts the id of this input
   * register to perform a View Change *)
  let i_register f b l (x:float) (y:float) (edge:float) svg =
    let g          = (container x y) |. E.click (fun _ _ _ -> f l) in
    let hex_str    = bs_to_hs b in
    let label      = text (edge *. 0.6) (edge *. 0.2) (edge /. 7.5) "" l in
    let type_label = text (edge *. 0.1) (edge *. 0.2) (edge /. 7.5) "" "I" in
    let bit_vals   = (text (edge *. 0.5) (edge *. 0.5) (edge /. 7.5) l hex_str) in
    let frame      = rect_c edge edge 0. 0. 2. 2. in
    svg |- (g |- frame |- bit_vals |- label |- type_label)

  (* Compilation Area
   * NOTE: This takes in a function that accepts a reference to a circuit
   * in order to build the circuit and place it at that reference, followed
   * by a rendering of that circuit (View Change) *)
  let compile_area (f:unit -> unit) thing1 thing2 =
    static "div"
    |. str attr "class" "initial"
    |. seq [
        (static "textarea"
        |. str attr "class" "code"
        |. int attr "rows" 10
        |. int attr "cols" 50);
        (static "button"
        |. html (fun _ _ _ -> "Compile")
        |. str attr "class" "compile-btn"
        |. E.click (fun _ _ _ -> f ()));
        (static "div"
        |. str attr "class" "debug-output"
        |. html (fun _ _ _ -> "Debug output"));thing1;thing2]

  (* Step Button
   * NOTE: This takes in a function that steps & updates the view dependent
   * on whether a circuit reference is None or not (View Change per reg) *)
  let step_btn (f:unit -> unit) =
    static "button"
    |. html (fun _ _ _ -> "Step")
    |. str attr "class" "step-btn"
    |. E.click (fun _ _ _ -> f ())

end


(* Include all of the above modules *)
include Wiring
include Gates
include SubBits
include Registers
include Comparators
include Arithmetics
include Miscs
include Triggers
