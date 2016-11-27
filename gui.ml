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

  let one = arith_nxor 0. 50. 60. in
  let two = arith_nxor 0. 150. 40. in
  let logic_and = logical_and 0. 250. 60. in
  let logic_or = logical_or 0. 350. 40. in
  let nottt = arith_not 0. 450. 60. in
  let sub_seq_lol = sub_seq_c 0. 550. 40. 1 24 in

  (* Construct our view *)
  (container
    |> one
    |> two
    |> logic_and
    |> logic_or
    |> nottt
    |> sub_seq_lol)

;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 1000; height = 700 } 50) ()
