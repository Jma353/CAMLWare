
(* RI: Except within constructors, this type is treated as IMMUTABLE *)
type bitstream = bool array

let max_length = 32

let and_bits b1 b2 =
  b1 && b2

let or_bits b1 b2 =
  b1 || b2

let xor_bits b1 b2 =
  (b1 && (not b2)) || ((not b1) && b2)

let length = Array.length

let create = Array.of_list

let singleton (b:bool) : bitstream = Array.make 1 b

let nth (b:bitstream) (n:int) : bitstream = singleton (Array.get b n)

let substream b n1 n2 = Array.sub b n1 (n2 - n1)

let is_zero = Array.for_all (fun x -> x = false)

let is_negative b = b.(length b - 1)

let zeros n = Array.make n false

let ones n = Array.make n true

let one n =
  let z = zeros n in
  z.(0) <- true; z

let sign_extend n b =
  Array.append b (Array.make (n - length b) (is_negative b))

let zero_extend n b =
  Array.append b (Array.make (n - length b) false)

let extend_matching extender b1 b2 =
  let resized1 = extender (max (length b1) (length b2)) b1 in
  let resized2 = extender (max (length b1) (length b2)) b2 in
  (resized1,resized2)

let set b n value =
  let c = Array.copy b in
  c.(n) <- value; c

let concat = Array.append

let reduce op b =
  if length b = 1 then b
  else let rem = Array.sub b 1 (length b - 1) in
    singleton (Array.fold_left (op) b.(0) rem)

let rec bitwise_binop op b1 b2 =
  if length b1 <> length b2 then
    let (r1,r2) = extend_matching (zero_extend) b1 b2 in
    bitwise_binop (op) r1 r2
  else Array.map2 (op) b1 b2

let logical_binop op b1 b2 =
  let r1 = reduce (||) b1 in
  let r2 = reduce (||) b2 in
  singleton (op r1.(0) r2.(0))

let bitwise_not =
  Array.map (not)

let logical_not b =
  let r = reduce (||) b in
  singleton (not r.(0))

let rec adder c_in b1 b2 n out =
  if n = length out then out else
    let x = xor_bits b1.(n) b2.(n) in
    let s = xor_bits x c_in in
    let c_out = c_in && x || b1.(n) && b2.(n) in
    out.(n) <- s;
    adder c_out b1 b2 (n+1) out

let add b1 b2 =
  let (r1,r2) = extend_matching (sign_extend) b1 b2 in
  let out = Array.make (length r1) false in
  adder false r1 r2 0 out

let subtract b1 b2 =
  let (r1,r2) = extend_matching (sign_extend) b1 b2 in
  let out = Array.make (length r1) false in
  adder true r1 (bitwise_not r2) 0 out

let negate b =
  let out = Array.make (length b) false in
  adder true (zeros (length b)) (bitwise_not b) 0 out

let rev_string s =
  String.init (String.length s)
    (fun x -> String.get s (String.length s - 1 - x))

let rec bitstream_of_string_helper (s:Scanf.Scanning.in_channel) (arr:bitstream)
    (n:int) (helper:Scanf.Scanning.in_channel -> char option -> bitstream -> int -> bitstream) : bitstream =
  if (n >= Array.length arr) then arr else
    let c = try Scanf.bscanf s "%c" (fun x -> Some x) with End_of_file -> None
    in helper s c arr n

let bitstream_of_string (s:string) (base:string)
    (helper:Scanf.Scanning.in_channel -> char option -> bitstream -> int -> bitstream) : bitstream =
  let split = Str.split (Str.regexp base) s in
  let (l,str) = if List.length split = 2
    then (int_of_string (List.hd split), List.hd (List.tl split))
    else (max_length, List.hd split) in
  let arr = Array.make l false in
  let schan = Scanf.Scanning.from_string (rev_string str) in
  bitstream_of_string_helper schan arr 0 (helper)

let bitstream_of_binstring s =
  let rec helper =
    fun schan c arr n -> match c with
      | None -> arr
      | Some '1' -> arr.(n) <- true;
        bitstream_of_string_helper schan arr (n+1) (helper)
      | _ -> bitstream_of_string_helper schan arr (n+1) (helper) in
  bitstream_of_string s "'b" (helper)

let bitstream_of_hexstring s =
  let eval_hex =
    fun h arr n ->
      let l = length arr - 1 - n in
      let setn0 = fun () -> arr.(n) <- true in
      let setn1 = fun () -> if l >= 1 then arr.(n+1) <- true else () in
      let setn2 = fun () -> if l >= 2 then arr.(n+2) <- true else () in
      let setn3 = fun () -> if l >= 3 then arr.(n+3) <- true else () in
      match h with
      | '1' -> setn0()
      | '2' -> setn1()
      | '3' -> setn0(); setn1()
      | '4' -> setn2()
      | '5' -> setn0(); setn2()
      | '6' -> setn1(); setn2()
      | '7' -> setn0(); setn1(); setn2()
      | '8' -> setn3()
      | '9' -> setn0(); setn3()
      | 'A' | 'a' -> setn1(); setn3()
      | 'B' | 'b' -> setn0(); setn1(); setn3()
      | 'C' | 'c' -> setn2(); setn3()
      | 'D' | 'd' -> setn0(); setn2(); setn3()
      | 'E' | 'e' -> setn1(); setn2(); setn3()
      | 'F' | 'f' -> setn0(); setn1(); setn2(); setn3()
      | _ -> ()
  in
  let rec helper =
    fun schan c arr n -> match c with
      | None -> arr
      | Some h -> (eval_hex h arr n);
        bitstream_of_string_helper schan arr (n+4) (helper)
  in
  bitstream_of_string s "'x" (helper)

let bitstream_of_decstring s =
  let split = Str.split (Str.regexp "'d") s in
  let (l,str) = if List.length split = 2
    then (int_of_string (List.hd split), List.hd (List.tl split))
    else (max_length, List.hd split) in
  let dec = int_of_string str in
  let abs_val = abs dec in
  let hex = Format.sprintf "%i'x%X" l abs_val in
  let bits = bitstream_of_hexstring hex in
  if dec < 0 then negate bits else bits

let bitstream_of_integer n =
  bitstream_of_decstring (Format.sprintf "%i'd%i" max_length n)

let bitstream_to_binstring b =
  let bits = String.init (length b)
      (fun i -> if b.(length b - 1 - i) then '1' else '0') in
  Format.sprintf "%i'b%s" (length b) bits

let bitstream_to_hexstring b =
  let l = length b in
  let ext =
    if l mod 4 <> 0
    then zero_extend (l + (4 - (l mod 4))) b
    else b in
  let new_l = length ext in
  let bits = String.init (new_l/4)
      (fun i ->
         let index = new_l - 1 - 4*i in
         let digit = (ext.(index),ext.(index-1),ext.(index-2),ext.(index-3)) in
         match digit with
         | (false,false,false,false) -> '0'
         | (false,false,false,true)  -> '1'
         | (false,false,true,false)  -> '2'
         | (false,false,true,true)   -> '3'
         | (false,true,false,false)  -> '4'
         | (false,true,false,true)   -> '5'
         | (false,true,true,false)   -> '6'
         | (false,true,true,true)    -> '7'
         | (true,false,false,false)  -> '8'
         | (true,false,false,true)   -> '9'
         | (true,false,true,false)   -> 'A'
         | (true,false,true,true)    -> 'B'
         | (true,true,false,false)   -> 'C'
         | (true,true,false,true)    -> 'D'
         | (true,true,true,false)    -> 'E'
         | (true,true,true,true)     -> 'F') in
  Format.sprintf "%i'x%s" l bits

let bitstream_to_integer_unsigned b =
  fst (Array.fold_left
         (fun (acc,power) bit ->
            if bit
            then (acc+power,2*power)
            else (acc, 2*power)) (0,1) b)

let bitstream_to_integer_signed b =
  let (i,_,_) =
    (Array.fold_left
       (fun (acc,power,count) bit ->
          if bit
          then
            if count = length b
            then (acc-power,2*power,count+1)
            else (acc+power,2*power,count+1)
          else (acc,2*power,count+1)) (0,1,1) b) in i

let bitstream_to_decstring_signed b =
  Format.sprintf "%i'd%i" (length b) (bitstream_to_integer_signed b)

let bitstream_to_decstring_unsigned b =
  Format.sprintf "%i'd%i" (length b) (bitstream_to_integer_unsigned b)

let shift_left b1 b2 =
  let n = bitstream_to_integer_unsigned b2 in
  concat (zeros n) (Array.sub b1 0 (length b1 - n))

let shift_right_logical b1 b2 =
  let n = bitstream_to_integer_unsigned b2 in
  concat (Array.sub b1 n (length b1 - n)) (zeros n)

let shift_right_arithmetic b1 b2 =
  let n = bitstream_to_integer_unsigned b2 in
  concat (Array.sub b1 n (length b1 - n)) (Array.make n (is_negative b1))

let relation comparator b1 b2 =
  let n1 = bitstream_to_integer_signed b1 in
  let n2 = bitstream_to_integer_signed b2 in
  singleton (comparator n1 n2)

let less_than = relation (<)

let greater_than = relation (>)

let equals = relation (=)

let format_bitstream f b =
  Format.fprintf f "%s" (bitstream_to_binstring b)
