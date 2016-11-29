open Parse
open Circuit
open Simulator 
open Bitstream  
open Combinational 
open OUnit2

let test name v1 v2 =
  name >:: (fun _ -> assert_equal v1 v2)

let create_reg_list name s1 n lst  = 
    test name lst (s1 |> parse_circuit_no_errors |> step_n n |> register_values|> StringMap.map (bitstream_to_hexstring) |> StringMap.bindings)

(*
let create_change_input name s1 n lst id val = 
    test name lst (s1 |> parse_circuit_no_errors |> step_n n |> register_values |> StringMap.map (bitstream_to_hexstring) |> StringMap.bindings)
*)

let change_input_test name s1 n lst id_vals = 
    let circ = s1 |> parse_circuit_no_errors in 
    let rec change lst circ = 
        match lst with 
        | [] -> circ 
        | h::t -> let nc = change_input (fst h) (snd h) circ in change t nc in 
    let nc = change id_vals circ in 
    test name lst (nc |> step_n n |> register_values|> StringMap.map (bitstream_to_hexstring) |> StringMap.bindings)


let tests = [
    create_reg_list "input no step" "input A[32]" 0 [("A", "32'x00000000")];
    create_reg_list "input 1 step" "input A[32]" 1 [("A", "32'x00000000")];
    create_reg_list "input output no step" "input A[32] output B[32] = A" 0 [("A", "32'x00000000"); ("B", "32'x00000000")];
    
    change_input_test "change_input" "input A[32]" 0 
                [("A", "32'x00000001")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "change_input" "input A[32] output B[32] = A" 0 
                [("A", "32'x00000001");("B", "32'x00000001")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];
    
    change_input_test "|" 
        "input A[32] register C[32] = 32'd10 register B[32] = A | C" 0 
                [("A", "32'x00000001");("B", "32'x00000000");(
                    "C", "32'x00000000")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "| 1 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = A | C" 1 
                [("A", "32'x00000001");("B", "32'x00000001");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "| 2 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = A | C" 2 
                [("A", "32'x00000001");("B", "32'x00000001");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "| 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = A | C" 3 
                [("A", "32'x00000001");("B", "32'x0000000B");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "|| 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = A || C" 3 
                [("A", "32'x00000001");("B", "32'x00000001");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "& 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = A & C" 3 
                [("A", "32'x00000001");("B", "32'x00000000");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "&& 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = A && C" 3 
                [("A", "32'x00000001");("B", "32'x00000001");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "&& 3 step" 
        "input A[32] register C[32] = 32'd0 register B[32] = A && C" 3 
                [("A", "32'x00000001");("B", "32'x00000000");(
                    "C", "32'x00000000")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "! 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] =!C" 3 
                [("A", "32'x00000001");("B", "32'x00000000");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "~ 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] =~C" 3 
                [("A", "32'x00000001");("B", "32'xFFFFFFF5");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "^ 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] =A^C" 3 
                [("A", "32'x00000001");("B", "32'x0000000B");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "~^ 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] =A~^C" 3 
                [("A", "32'x00000001");("B", "32'xFFFFFFF4");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "~& 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] =A~&C" 3 
                [("A", "32'x00000001");("B", "32'xFFFFFFFF");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "~| 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] =A~|C" 3 
                [("A", "32'x00000001");("B", "32'xFFFFFFF4");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test "<< 3 step" 
        "input A[32] register C[32] = 32'd1 register B[32] =A<<C" 3 
                [("A", "32'x00000001");("B", "32'x00000002");(
                    "C", "32'x00000001")] 
                [("A",(bitstream_of_hexstring "32'x00000001"))];

    change_input_test ">> 3 step" 
        "input A[32] register C[32] = 32'd1 register B[32] =A>>C" 3 
                [("A", "32'x00000010");("B", "32'x00000008");(
                    "C", "32'x00000001")] 
                [("A",(bitstream_of_hexstring "32'x00000010"))];

    change_input_test ">>> + 3 step" 
        "input A[32] register C[32] = 32'd1 register B[32] =A>>>C" 3 
                [("A", "32'x00000010");("B", "32'x00000008");(
                    "C", "32'x00000001")] 
                [("A",(bitstream_of_hexstring "32'x00000010"))];

    change_input_test ">>> - 3 step" 
        "input A[32] register C[32] = 32'd1 register B[32] =A>>>C" 3 
                [("A", "32'xF0000010");("B", "32'xF8000008");(
                    "C", "32'x00000001")] 
                [("A",(bitstream_of_hexstring "32'xF0000010"))];

    change_input_test "+ 3 step" 
        "input A[32] register C[32] = 32'd12 register B[32] =A+C" 3 
                [("A", "32'x0000000A");("B", "32'x00000016");(
                    "C", "32'x0000000C")] 
                [("A",(bitstream_of_decstring "32'd10"))];

    change_input_test "- 3 step" 
        "input A[32] register C[32] = 32'd12 register B[32] =C - A" 3 
                [("A", "32'x0000000A");("B", "32'x00000002");(
                    "C", "32'x0000000C")] 
                [("A",(bitstream_of_decstring "32'd10"))];

    change_input_test "- equal 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = C - A" 3 
                [("A", "32'x0000000A");("B", "32'x00000000");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_decstring "32'd10"))];

     change_input_test "- neg 3 step" 
        "input A[32] register C[32] = 32'd10 register B[32] = C - A" 3 
                [("A", "32'x0000000C");("B", "32'xFFFFFFFE");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_decstring "32'd12"))];

    change_input_test "concat step1" 
        "input A[32] register C[32] = 32'd10 register B[32] = {A, C}" 3 
                [("A", "32'x0000000C");("B", "32'x0000000A");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_decstring "32'd12"))];

    change_input_test "concat step2" 
        "input A[32] register C[32] = 32'd10 register B[32] = {A[0-16], C[0-13]}" 3 
                [("A", "32'x0000000C");("B", "32'x0003000A");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_decstring "32'd12"))];

    change_input_test "substream" 
        "input A[32] register C[32] = 32'd10 register B[16] = A[0-15]" 3 
                [("A", "32'x0000000C");("B", "16'x000C");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_decstring "32'd12"))];

    change_input_test "mux" 
        "input A[32] register C[32] = 32'd10 register B[16] = if A == C then 16'd14 else 16'd15" 3 
                [("A", "32'x0000000C");("B", "16'x000F");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_decstring "32'd12"))];

    change_input_test "mux equal" 
        "input A[32] register C[32] = 32'd10 register B[16] = if A == C then 16'd0 else 16'd16" 3 
                [("A", "32'x0000000A");("B", "16'x0000");(
                    "C", "32'x0000000A")] 
                [("A",(bitstream_of_decstring "32'd10"))];

    change_input_test "fun" 
        "input A[32] register C[32] = 32'd12 fun sub(x[32],y[32])[32] = x + y register B[32] = sub(A,C)" 3 
                [("A", "32'x0000000A");("B", "32'x00000016");(
                    "C", "32'x0000000C")] 
                [("A",(bitstream_of_decstring "32'd10"))];

    change_input_test "fun" 
        "input A[32] register C[32] = 32'd12 fun sub(x[32],y[32])[32] = let z = x + y in z register B[32] = sub(A,C)" 3 
                [("A", "32'x0000000A");("B", "32'x00000016");(
                    "C", "32'x0000000C")] 
                [("A",(bitstream_of_decstring "32'd10"))];

    change_input_test "nth" 
        "input A[32] register C[32] = 32'd12 output B[32] = A[1]" 3 
                [("A", "32'x0000000A");("B", "32'x00000001");(
                    "C", "32'x0000000C")] 
                [("A",(bitstream_of_decstring "32'd10"))];

    
]

