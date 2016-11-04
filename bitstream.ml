open Base_conversions

(* Temporary implementation so the parser is testable *)

type bitstream = string

let length b =
  failwith "unimplemented"

let nth b n =
  failwith "unimplemented"

let substream b n1 n2 =
  failwith "unimplemented"

let is_zero b =
  failwith "unimplemented"

let negative b =
  failwith "unimplemented"

let rec replicate_string c n =
  if n = 0 then "" else
  c^(replicate_string c (n-1))

let zeros n =
  "0x"^(replicate_string "0" (if n mod 4 = 0 then n/4 else n/4 + 1))

let ones n =
  failwith "unimplemented"

let one n =
  failwith "unimplemented"

let singleton b =
  failwith "unimplemented"

let create bs =
  failwith "unimplemented"

let bitstream_of_binstring s =
  hexstring_of_binstring_unsigned s

let bitstream_of_hexstring s =
  s

let bitstream_of_decimal d =
  failwith "unimplemented"

let set b n value =
  failwith "unimplemented"

let replicate b n =
  failwith "unimplemented"

let concat b1 b2 =
  failwith "unimplemented"

let reduce op b =
  failwith "unimplemented"

let bitwise_binop op b1 b2 =
  failwith "unimplemented"

let logical_binop op b1 b2 =
  failwith "unimplemented"

let bitwise_not b =
  failwith "unimplemented"

let logical_not b =
  failwith "unimplemented"

let negate  b =
  failwith "unimplemented"

let add b1 b2 =
  failwith "unimplemented"

let subtract b1 b2 =
  failwith "unimplemented"

let shift_left b n =
  failwith "unimplemented"

let shift_right_logical b n =
  failwith "unimplemented"

let shift_right_arithmetic b n =
  failwith "unimplemented"

let less_than b1 b2 =
  failwith "unimplemented"

let greater_than b1 b2 =
  failwith "unimplemented"

let equals b1 b2 =
  failwith "unimplemented"

let format_bitstream f b =
  Format.fprintf f "%s" b
