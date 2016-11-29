(* OUnit test suite for Bitstream module *)
open OUnit2
open Bitstream

let strb = bitstream_to_binstring
let strx = bitstream_to_hexstring
let strds = bitstream_to_decstring_signed
let strdu = bitstream_to_decstring_unsigned

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
]
