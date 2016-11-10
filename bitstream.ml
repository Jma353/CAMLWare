open Base_conversions

(* Temporary implementation so the parser is testable *)

type bitstream = bool list 

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

let bitstream_of_binstring s =
  let rec make = function 
  | [] -> []
  | h::t -> if h = "1" then true::(make t) else if h = "0" then false::(make t) else make t
  in make (Str.split (Str.regexp "") s) 

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

let add b1 b2 =
  let carryout = negative b1 = negative b2 in 
  let rec add_hlpr carry l1 l2 =
  match (carry, l1, l2) with 
  | (x, [], []) -> if carryout then [x] else []
  | (false, false::l1s, false::l2s) -> false::(add_hlpr false l1s l2s)
  | (true, false::l1s, false::l2s) -> true::(add_hlpr false l1s l2s)
  | (true, true::l1s, false::l2s) -> false::(add_hlpr true l1s l2s)
  | (true, true::l1s, true::l2s) -> true::(add_hlpr true l1s l2s)
  | (true, false::l1s, true::l2s) -> false::(add_hlpr true l1s l2s)
  | (false, true::l1s, true::l2s) -> false::(add_hlpr true l1s l2s)
  | (false, true::l1s, false::l2s) -> true::(add_hlpr false l1s l2s)
  | (false, false::l1s, true::l2s) -> true::(add_hlpr false l1s l2s)
  | _ -> failwith "invalid input for add"
  in add_hlpr false b1 b2

let negate  b =
  let rec make lst = match lst with
  | [] -> []
  | h::t -> (not h)::(make t)
  in let bin = make b in
  add bin (one (List.length b))

let subtract b1 b2 =
  let added = add b1 (negate b2) in 
  if negative b1 = negative b2 then added else 
    let rec all_but_last = function 
      | [] -> []
      | x::[] -> []
      | h::t -> h::(all_but_last t)
    in all_but_last added

(* flag .. unsigned or signed? *)
let bitstream_of_decimal d =
  let rec divide q =
    if q / 2 = 0 then 
      if q mod 2 = 1 then [true] else [false]
    else 
      if q mod 2 = 1 then true::(divide (q / 2)) else false::(divide (q / 2))
  in let bin = divide d in 
  if d < 0 then (* twos complement *) negate bin
  else bin @ [false]

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
  if List.length b1 != List.length b2 then zeros 1 else 
    if (bitwise_binop (fun x y -> x = y) b1 b2) |> List.for_all (fun x -> x) 
      then ones 1 
    else zeros 1

let rec format_bitstream f b =
  match b with
  | [] -> ()
  | h::t -> if h then Format.fprintf f "%s%a" "1"
                        (format_bitstream) t
            else Format.fprintf f "%s%a" "0"
                        (format_bitstream) t 
