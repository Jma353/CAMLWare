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

  (* Test gates *)
  let one = red_nxor 0. 50. 60. in
  let two = mux2_c 0. 150. 60. in
  let three = sub_seq_c 0. 250. 60. 4 27 in
  let four = nth_c 0. 350. 60. 8 in

  (* Construct our view *)
  (container
    |> one
    |> two
    |> three
    |> four)


;;

let _ =
  run ~node:(Dom_html.document##body) (view { width = 1000; height = 700 } 50) ()
