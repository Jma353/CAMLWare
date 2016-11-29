(* OUnit test suite for Bitstream module *)
open OUnit2
open Bitstream

let strb = bitstream_to_binstring
let strx = bitstream_to_hexstring
let strds = bitstream_to_decstring_signed
let strdu = bitstream_to_decstring_unsigned
let to_ints = bitstream_to_integer_signed
let to_intu = bitstream_to_integer_unsigned

let bstrb = bitstream_of_binstring
let bstrx = bitstream_of_hexstring
let bstrd = bitstream_of_decstring
let from_int = bitstream_of_integer

let test name v1 v2 =
  name >:: (fun _ -> assert_equal v1 v2)

let tests = [
  (test "zeros 1 bin" "1'b0" (1 |> zeros |> strb));
  (test "zeros 1 hex" "1'x0" (1 |> zeros |> strx));
  (test "zeros 1 dec" "1'd0" (1 |> zeros |> strdu));
  (test "zeros 32 hex" "32'x00000000" (32 |> zeros |> strx));
  (test "ones 1 bin" "1'b1" (1 |> ones |> strb));
  (test "ones 1 hex" "1'x1" (1 |> ones |> strx));
  (test "ones 1 dec" "1'd1" (1 |> ones |> strdu));
  (test "ones 32 hex" "32'xFFFFFFFF" (32 |> ones |> strx));
  (test "one 1 bin" "1'b1" (1 |> one |> strb));
  (test "one 1 hex" "1'x1" (1 |> one |> strx));
  (test "one 1 dec" "1'd1" (1 |> one |> strdu));
  (test "one 32 hex" "32'x00000001" (32 |> one |> strx));
  (test "length zeros 1" 1 (1 |> zeros |> length));
  (test "length ones 1" 1 (1 |> ones |> length));
  (test "length zeros 5" 5 (5 |> zeros |> length));
  (test "length one 5" 5 (5 |> one |> length));
  (test "length zeros 32" 32 (32 |> zeros |> length));
  (test "is_zero zeros 1" true (1 |> zeros |> is_zero));
  (test "is_zero zeros 32" true (32 |> zeros |> is_zero));
  (test "is_zero ones 1" false (1 |> ones |> is_zero));
  (test "is_zero ones 32" false (32 |> ones |> is_zero));
  (test "is_negative zeros 32" false (32 |> zeros |> is_negative));
  (test "is_negative one 32" false (32 |> one |> is_negative));
  (test "is_negative ones 32" true (32 |> ones |> is_negative));
  (test "singleton 0" "1'b0" (false |> singleton |> strb));
  (test "singleton 1" "1'b1" (true |> singleton |> strb));
  (test "binstring basic" "1'b1" ("1'b1" |> bstrb |> strb));
  (test "binstring truncates" "1'b0" ("1'b10" |> bstrb |> strb));
  (test "binstring extends" "2'b01" ("2'b1" |> bstrb |> strb));
  (test "default length" "32'x00000002" ("'b10" |> bstrb |> strx));
  (test "default length2" 32 ("'b1" |> bstrb |> length));
  (test "set0" "5'b00001" ((set (zeros 5) 0 true) |> strb));
  (test "set1" "5'b00010" ((set (zeros 5) 1 true) |> strb));
  (test "set2" "5'b00100" ((set (zeros 5) 2 true) |> strb));
  (test "set3" "5'b01000" ((set (zeros 5) 3 true) |> strb));
  (test "set4" "5'b10000" ((set (zeros 5) 4 true) |> strb));
  (test "nth1" "1'b1" ((nth (one 5) 0) |> strb));
  (test "nth2" "1'b0" ((nth (one 5) 1) |> strb));
  (test "substream1" "3'b001" ((substream (one 5) 0 2) |> strb));
  (test "substream2" "3'b000" ((substream (one 5) 1 3) |> strb));
  (test "substream3" "1'b1" ((substream (one 5) 0 0) |> strb));
  (test "concat1" "5'b11100" ((concat (zeros 2) (ones 3)) |> strb));
  (test "reduce and1" "1'b1" (5 |> ones |> (reduce (&&)) |> strb));
  (test "reduce and2" "1'b0" ("5'b11011" |> bstrb |> (reduce (&&)) |> strb));
  (test "reduce or1" "1'b1" ("5'b00100" |> bstrb |> (reduce (||)) |> strb));
  (test "reduce or2" "1'b0" (5 |> zeros |> (reduce (||)) |> strb));
  (test "bitwise not1" "32'xFFFFFFFF" (32 |> zeros |> bitwise_not |> strx));
  (test "bitwise not2" "32'x00000000" (32 |> ones |> bitwise_not |> strx));
  (test "negate1" "32'x00000001" (32 |> ones |> negate |> strx));
  (test "negate2" "32'x00000000" (32 |> zeros |> negate |> strx));
  (test "sll0" "8'b11111111" ((shift_left (ones 8) ("8'd0" |> bstrd)) |> strb));
  (test "sll1" "8'b11111110" ((shift_left (ones 8) ("8'd1" |> bstrd)) |> strb));
  (test "sll2" "8'b11111100" ((shift_left (ones 8) ("8'd2" |> bstrd)) |> strb));
  (test "sll3" "8'b11111000" ((shift_left (ones 8) ("8'd3" |> bstrd)) |> strb));
  (test "sll4" "8'b11110000" ((shift_left (ones 8) ("8'd4" |> bstrd)) |> strb));
  (test "sll5" "8'b11100000" ((shift_left (ones 8) ("8'd5" |> bstrd)) |> strb));
  (test "sll6" "8'b11000000" ((shift_left (ones 8) ("8'd6" |> bstrd)) |> strb));
  (test "sll7" "8'b10000000" ((shift_left (ones 8) ("8'd7" |> bstrd)) |> strb));
  (test "sll8" "8'b00000000" ((shift_left (ones 8) ("8'd8" |> bstrd)) |> strb));
  (test "srl0" "8'b11111111" ((shift_right_logical (ones 8) ("8'd0" |> bstrd))
                              |> strb));
  (test "srl1" "8'b01111111" ((shift_right_logical (ones 8) ("8'd1" |> bstrd))
                              |> strb));
  (test "srl2" "8'b00111111" ((shift_right_logical (ones 8) ("8'd2" |> bstrd))
                              |> strb));
  (test "srl3" "8'b00011111" ((shift_right_logical (ones 8) ("8'd3" |> bstrd))
                              |> strb));
  (test "srl4" "8'b00001111" ((shift_right_logical (ones 8) ("8'd4" |> bstrd))
                              |> strb));
  (test "srl5" "8'b00000111" ((shift_right_logical (ones 8) ("8'd5" |> bstrd))
                              |> strb));
  (test "srl6" "8'b00000011" ((shift_right_logical (ones 8) ("8'd6" |> bstrd))
                              |> strb));
  (test "srl7" "8'b00000001" ((shift_right_logical (ones 8) ("8'd7" |> bstrd))
                              |> strb));
  (test "srl8" "8'b00000000" ((shift_right_logical (ones 8) ("8'd8" |> bstrd))
                              |> strb));
  (test "sra0" "4'b0111" ((shift_right_arithmetic ("4'b0111" |> bstrb)
                             ("4'd0" |> bstrd)) |> strb));
  (test "sra1 pos" "4'b0011" ((shift_right_arithmetic ("4'b0111" |> bstrb)
                              ("4'd1" |> bstrd)) |> strb));
  (test "sra2 neg" "4'b1111" ((shift_right_arithmetic ("4'b1111" |> bstrb)
                                 ("4'd2" |> bstrd)) |> strb));
  (test "add1" "32'd5" ((add ("'d2" |> bstrd) ("'d3" |> bstrd)) |> strds));
  (test "add2" "32'd5" ((add ("'d3" |> bstrd) ("'d2" |> bstrd)) |> strds));
  (test "add3" "32'd-1" ((add ("'d2" |> bstrd) ("'d-3" |> bstrd)) |> strds));
  (test "subtract1" "32'd5" ((subtract ("'d2" |> bstrd) ("'d-3" |> bstrd))
                             |> strds));
  (test "gt1" "1'b1" ((greater_than ("'d3" |> bstrd) ("'d2" |> bstrd))|> strb));
  (test "gt2" "1'b0" ((greater_than ("'d2" |> bstrd) ("'d3" |> bstrd))|> strb));
  (test "gt3" "1'b0" ((greater_than ("'d2" |> bstrd) ("'d2" |> bstrd))|> strb));
  (test "lt1" "1'b0" ((less_than ("'d3" |> bstrd) ("'d2" |> bstrd))|> strb));
  (test "lt2" "1'b1" ((less_than ("'d2" |> bstrd) ("'d3" |> bstrd))|> strb));
  (test "lt3" "1'b0" ((less_than ("'d2" |> bstrd) ("'d2" |> bstrd))|> strb));
  (test "eq1" "1'b0" ((equals ("'d3" |> bstrd) ("'d2" |> bstrd))|> strb));
  (test "eq2" "1'b0" ((equals ("'d2" |> bstrd) ("'d3" |> bstrd))|> strb));
  (test "eq3" "1'b1" ((equals ("'d2" |> bstrd) ("'d2" |> bstrd))|> strb));
]
