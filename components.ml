open D3


let register w h svg =
  let frame =
    append "rect"
    |. int attr "width" w
    |. int attr "height" h
    |. int attr "x" 0
    |. int attr "y" 0
    |. str style "stroke" "black"
    |. str style "fill" "transparent"
    |. int style "stroke-width" 1
  in
  let line1 =
    append "line"
    |. int attr "x1" 0
    |. int attr "y1" (int_of_float (0.75 *. float_of_int h))
    |. int attr "x2" (int_of_float (0.15 *. float_of_int w))
    |. int attr "y2" (int_of_float (0.80 *. float_of_int h))
    |. int style "stroke-width" 1
    |. str style "stroke" "black"
  in
  let line2 =
    append "line"
    |. int attr "x1" (int_of_float (0.15 *. float_of_int w))
    |. int attr "y1" (int_of_float (0.80 *. float_of_int h))
    |. int attr "x2" 0
    |. int attr "y2" (int_of_float (0.85 *. float_of_int h))
    |. int style "stroke-width" 1
    |. str style "stroke" "black"
  in
  ((svg |- frame) |- line1) |- line2
