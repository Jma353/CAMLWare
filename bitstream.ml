open Base_conversions

(* Temporary implementation so the parser is testable *)

type bitstream = bool list

let max_bits = 64

let length b =
  List.length b

let create bs =
  bs

let nth (b: bitstream) (n: int) =
  (List.nth b n)::[]

let substream b n1 n2 =
  let rec make lst i =
    match lst with
      | [] -> []
      | h::t -> if i >= n1 && i <= n2 then h::(make t (i + 1))
                else make t (i + 1)
    in make b 0


let is_zero b =
  List.for_all (fun x -> x = false) b


let negative b =
  List.nth b ((List.length b) - 1) (* most significant bit is the last bit *)


let zeros n =
  if n < 0 then [] else
    let rec make = function
      | 0 -> []
      | x -> false::(make (x - 1))
    in make n

let ones n =
  if n < 0 then [] else
    let rec make = function
      | 0 -> []
      | x -> true::(make (x - 1))
    in make n

let one n =
  if n <= 0 then [] else true::(zeros (n - 1))

let singleton b =
  [b]

let bitstream_to_binstring b =
  let rec make lst =
    match lst with
    | [] -> ""
    | true::t -> (make t) ^ "1"
    | false::t -> (make t) ^ "0"
  in "0b" ^ (make b)

let bitstream_of_binstring s =
  let bin = if String.length s >= 3 && String.sub s 0 2 = "0b"
            then String.sub s 2 (String.length s - 2) else s in
  let rec make = function
  | [] -> []
  | h::t -> if h = "1" then true::(make t) else if h = "0"
            then false::(make t)
            else failwith "incorrect bitstream_of_binstring input"
  in make (Str.split (Str.regexp "") bin) |> List.rev

(* flag .. unsigned or signed? *)
let bitstream_of_hexstring s =
  let binary = binstring_of_hexstring s in
  bitstream_of_binstring binary

let set b n value =
  (substream b 0 (n - 1)) @ [value] @ (substream b (n + 1) (List.length b))

let replicate b n =
  let rec make = function
    | 0 -> []
    | x -> b @ (make (x - 1))
  in make n

let concat b1 b2 =
  b1 @ b2

let reduce (op: bool -> bool -> bool) (b: bitstream) =
  if b = [] then []
  else (List.fold_left (fun x acc -> op x acc) (List.hd b) (List.tl b))::[]

let zero_extend n bitstream =
  let r = List.rev bitstream in
  let rec make needs lst =
    match needs with
    | 0 -> lst
    | x -> (false::(make (x - 1) lst))
  in List.rev (make (n - (length bitstream)) r)

let sign_extend n b =
  let r = List.rev b in
  let sign = List.hd r in
  let rec make needs lst =
    match needs with
    | 0 -> lst
    | x -> (sign::(make (x - 1) lst))
  in List.rev (make (n - (length b)) r)

let format_logical input1 input2 =
  let l1 = length input1 in
  let l2 = length input2 in
  if (l1 > l2)
  then (input1, (zero_extend l1 input2))
  else if (l1 = l2) then (input1, input2)
  else ((zero_extend l2 input1), input2)

let format_arithmetic input1 input2 =
  let l1 = length input1 in
  let l2 = length input2 in
  if (l1 > l2)
  then (input1, (sign_extend l1 input2))
  else if (l1 = l2) then (input1, input2)
  else ((sign_extend l2 input1), input2)

let bitwise_binop op b1 b2 =
  let rec make l1 l2 = match (l1, l2) with
    | ([], []) -> []
    | (y::ys, x::xs) -> (op y x)::(make ys xs)
    | _ -> failwith "incorrect bitwise_binop inputs"
  in make b1 b2

let logical_binop op (b1: bitstream) (b2: bitstream) =
  let n = List.length b1 in
  match (op (not (is_zero b1)) (not (is_zero b2))) with
  | true -> ones n
  | false -> zeros n

let bitwise_not b =
  List.map (fun x -> not x) b

let logical_not b =
  let n = List.length b in
  if (is_zero b) then ones n else zeros n

let adder b1 b2 carryout=
  let rec add_hlpr carry l1 l2 =
  match (carry, l1, l2) with
  | (x, [], []) -> []
  | (false, false::l1s, false::l2s) -> false::(add_hlpr false l1s l2s)
  | (true, false::l1s, false::l2s) -> true::(add_hlpr false l1s l2s)
  | (true, true::l1s, false::l2s) -> false::(add_hlpr true l1s l2s)
  | (true, true::l1s, true::l2s) -> true::(add_hlpr true l1s l2s)
  | (true, false::l1s, true::l2s) -> false::(add_hlpr true l1s l2s)
  | (false, true::l1s, true::l2s) -> false::(add_hlpr true l1s l2s)
  | (false, true::l1s, false::l2s) -> true::(add_hlpr false l1s l2s)
  | (false, false::l1s, true::l2s) -> true::(add_hlpr false l1s l2s)
  | _ -> failwith "invalid input for add"
  in add_hlpr carryout b1 b2

let add b1 b2 =
  let (new1, new2) = format_arithmetic b1 b2 in
  let added = adder new1 new2 false in
  if length added > max_bits then substream added 0 63 else added

let negate b =
  if (length b = 1) then bitwise_not b
  else (add (bitwise_not b) (create [true; false]))

let subtract b1 b2 =
  let (new1, new2) = format_arithmetic b1 b2 in
  let added = adder new1 (bitwise_not new2) true in
  if length added > max_bits then substream added 0 63 else added

let bitstream_of_decimal d =
  let rec divide q =
    if q / 2 = 0 then
      if q mod 2 = 1 then [true] else [false]
    else
      if q mod 2 = 1 then true::(divide (q / 2)) else false::(divide (q / 2))
  in let bin = zero_extend max_bits (divide (Pervasives.abs d)) in
  if d < 0 then negate bin (* twos complement *)
  else bin

let decimal_of_bitstream b =
  let rec add_helper b total factor =
    match b with
    | [] -> total
    | h::t ->
      if (h) then add_helper t (total+factor) (factor*2)
      else add_helper t total (factor*2)
  in add_helper b 0 1

let rec shift_left_helper (b: bitstream) n (fill_in: bool) =
  match n with
  | 0 -> b
  | x -> fill_in::(shift_left_helper b (x - 1) fill_in)

let shift_left b n =
  let shift = if negative n then failwith "incorrect input for shift_left"
              else (dec_of_binstring_unsigned (bitstream_to_binstring n)) in
  let shifted = shift_left_helper b shift false in
  if length shifted > max_bits then substream shifted 0 63 else shifted

let rec shift_right_helper (b: bitstream) n (fill_in: bool) =
  List.rev (shift_left_helper (List.rev b) n fill_in)

let shift_right_logical b n =
  let shift = if negative n then failwith "incorrect input for shift_left"
              else (dec_of_binstring_unsigned (bitstream_to_binstring n)) in
  let shifted = shift_right_helper b shift false in
  if length shifted > max_bits
  then substream shifted (length shifted - max_bits) (length shifted) else shifted

let shift_right_arithmetic b n =
  let shift = if negative n then failwith "incorrect input for shift_left"
              else (dec_of_binstring_unsigned (bitstream_to_binstring n)) in
  let shifted = shift_right_helper b shift (negative b) in
  if length shifted > max_bits
  then substream shifted (length shifted - max_bits) (length shifted) else shifted


let less_than b1 b2 =
  let dec_b1 = if negative b1
              then (dec_of_binstring_signed (bitstream_to_binstring b1))
              else (dec_of_binstring_unsigned (bitstream_to_binstring b1)) in
  let dec_b2 = if negative b2
              then (dec_of_binstring_signed (bitstream_to_binstring b2))
              else (dec_of_binstring_unsigned (bitstream_to_binstring b2)) in
  if dec_b1 < dec_b2 then ones 1 else zeros 1


let greater_than b1 b2 =
   let dec_b1 = if negative b1
              then (dec_of_binstring_signed (bitstream_to_binstring b1))
              else (dec_of_binstring_unsigned (bitstream_to_binstring b1)) in
  let dec_b2 = if negative b2
              then (dec_of_binstring_signed (bitstream_to_binstring b2))
              else (dec_of_binstring_unsigned (bitstream_to_binstring b2)) in
  if dec_b1 > dec_b2 then ones 1 else zeros 1

let equals b1 b2 =
  if List.length b1 != List.length b2 then zeros 1 else
    if (bitwise_binop (fun x y -> x = y) b1 b2) |> List.for_all (fun x -> x)
      then ones 1
    else zeros 1

let rec format_bitstream_helper f b =
  match b with
  | [] -> ()
  | h::t -> if h then Format.fprintf f "%s%a" "1"
                        (format_bitstream_helper) t
            else Format.fprintf f "%s%a" "0"
                        (format_bitstream_helper) t
let format_bitstream f b =
  format_bitstream_helper f (List.rev b)
