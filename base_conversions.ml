let hex_digit_to_bin s =
  match s with
  | '0' -> "0000"
  | '1' -> "0001"
  | '2' -> "0010"
  | '3' -> "0011"
  | '4' -> "0100"
  | '5' -> "0101"
  | '6' -> "0110"
  | '7' -> "0111"
  | '8' -> "1000"
  | '9' -> "1001"
  | 'A' | 'a' -> "1010"
  | 'B' | 'b' -> "1011"
  | 'C' | 'c' -> "1100"
  | 'D' | 'd' -> "1101"
  | 'E' | 'e' -> "1110"
  | 'F' | 'f' -> "1111"
  | c -> failwith ((String.make 1 c)^" is not valid hexadecimal")

let bin_group_to_hex s =
  match s with
  | "0000" -> "0"
  | "0001" -> "1"
  | "0010" -> "2"
  | "0011" -> "3"
  | "0100" -> "4"
  | "0101" -> "5"
  | "0110" -> "6"
  | "0111" -> "7"
  | "1000" -> "8"
  | "1001" -> "9"
  | "1010" -> "A"
  | "1011" -> "B"
  | "1100" -> "C"
  | "1101" -> "D"
  | "1110" -> "E"
  | "1111" -> "F"
  | c -> failwith (c^" is not valid binary")

let rec string_to_char_list_helper l s =
  if s = "" then l else
    let c = String.get s (String.length s - 1) in
    let sub = String.sub s 0 (String.length s - 1) in
    string_to_char_list_helper (c::l) sub

let string_to_char_list s =
  string_to_char_list_helper [] s

let rec group_bin_by_fours_helper signed l s =
  match String.length s with
  | 0 -> l
  | 1 -> let ext =
           if signed then (if s = "1" then "111" else "000")
           else "000" in (ext^s)::l
  | 2 -> let ext =
           if signed then (if String.get s 0 = '1' then "11" else "00")
           else "00" in (ext^s)::l
  | 3 -> let ext =
           if signed then (if String.get s 0 = '1' then "1" else "0")
           else "0" in (ext^s)::l
  | _ ->
    let c = String.sub s (String.length s - 4) 4 in
    let sub = String.sub s 0 (String.length s - 4) in
    group_bin_by_fours_helper signed (c::l) sub

let group_bin_by_fours_signed s =
  group_bin_by_fours_helper true [] s

let group_bin_by_fours_unsigned s =
  group_bin_by_fours_helper false [] s

let hexstring_of_binstring_helper signed s =
  let bin =
    if String.length s >= 3 && String.sub s 0 2 = "0b"
    then String.sub s 2 (String.length s - 2)
    else s in
  let grouper =
    if signed
    then group_bin_by_fours_signed
    else group_bin_by_fours_unsigned in
  let lst = grouper bin in
  lst |> (List.map bin_group_to_hex) |> (List.fold_left (^) "0x")

let hexstring_of_binstring_signed s =
  hexstring_of_binstring_helper true s

let hexstring_of_binstring_unsigned s =
  hexstring_of_binstring_helper false s

let binstring_of_hexstring s =
    let hex =
      if String.length s >= 3 && String.sub s 0 2 = "0x"
      then String.sub s 2 (String.length s - 2)
      else s in
    let lst = string_to_char_list hex in
    lst |> (List.map hex_digit_to_bin) |> (List.fold_left (^) "0b")

let dec_of_binstring_helper signed s =
  let bin =
    if String.length s >= 3 && String.sub s 0 2 = "0b"
    then String.sub s 2 (String.length s - 2)
    else s in
  let lst = string_to_char_list bin in
  match
  (List.fold_right
    (fun digit (sum,mult,index,total) ->
       if digit = '0'
       then (sum,2*mult,index+1,total)
       else if digit = '1'
       then (sum+mult*(if index = total && signed then (-1) else 1),
             2*mult,index+1,total)
       else failwith ((String.make 1 digit)^ " is not valid binary"))
   lst (0,1,1,String.length bin)) with
  | (sum,_,_,_) -> sum

let dec_of_binstring_signed s =
  dec_of_binstring_helper true s

let dec_of_binstring_unsigned s =
  dec_of_binstring_helper false s

let dec_of_hexstring_signed s =
  dec_of_binstring_signed (binstring_of_hexstring s)

let dec_of_hexstring_unsigned s =
  dec_of_binstring_unsigned (binstring_of_hexstring s)
