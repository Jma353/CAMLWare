(* OUnit testing suite for Circuit.Simulator *)

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

let change_input_test name s1 n lst id_vals =
    let circ = s1 |> parse_circuit_no_errors in
    let rec change lst circ =
        match lst with
        | [] -> circ
        | h::t -> let nc = change_input (fst h) (snd h) circ in change t nc in
    let nc = change id_vals circ in
    test name lst (nc |> step_n n |> register_values|> StringMap.map (bitstream_to_hexstring) |> StringMap.bindings)

let state_machine_tester name s1 lst id_vals =
    let circ = s1 |> parse_circuit_no_errors in
    let rec change lst circ =
       match lst with
        | [] -> circ
        | h::t -> let nc = change_input (fst h) (snd h) circ in let nc1 = step nc in change t nc1
    in let nc = change id_vals circ in
    test name lst (nc |> register_values |> StringMap.map (bitstream_to_hexstring) |> StringMap.bindings)


(* STATE MACHINE EXAMPLE TESTING *)
let inf_string =  "input in_channel[1]
            register state[3] = next()
            output out_channel[1] = state == 3'b100
            fun next()[3] =
              if state == 3'd0 then
                if in_channel
                then 3'd1
                else 3'd0
              else if state == 3'd1 then
                if in_channel
                then 3'd1
                else 3'd2
              else if state == 3'd2 then
                if in_channel
                then 3'd1
                else 3'd3
              else if state == 3'd3 then
                if in_channel
                then 3'd4
                else 3'd0
              else
                if in_channel
                then 3'd1
                else 3'd0"

let opt_string = "input in_channel[1]
    register state[3] = next()
    output out_channel[1] = state[2]
    fun next()[3] = {
    state[1] & state[0] & in_channel,
    ~state[1] & state[0] & ~in_channel | state[1] & ~state[0] & ~in_channel,
    ~state[1] & in_channel | state[1] & ~state[0]}"

let test_state_machine sm_string = [
    change_input_test "state_machine" sm_string 0
        [("in_channel", "1'x0");("out_channel", "1'x0");("state", "3'x0")]
        [];

    change_input_test "state_machine step1" sm_string 1
        [("in_channel", "1'x0");("out_channel", "1'x0");("state", "3'x0")]
        [];

    state_machine_tester "state machine step2" sm_string
        [("in_channel", "1'x1");("out_channel", "1'x0");("state", "3'x1")]
        [("in_channel", bitstream_of_binstring "1'b1")];

    state_machine_tester "state machine step3" sm_string
        [("in_channel", "1'x1");("out_channel", "1'x0");("state", "3'x1")]
        [("in_channel", bitstream_of_binstring "1'b1"); ("in_channel", bitstream_of_binstring "1'b1")];

    test "state_machine step4" [("in_channel", "1'x0");("out_channel", "1'x0");("state", "3'x2")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

    test "state_machine step5" [("in_channel", "1'x0");("out_channel", "1'x0");("state", "3'x3")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

    test "state_machine step6" [("in_channel", "1'x1");("out_channel", "1'x1");("state", "3'x4")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

     test "state_machine step7" [("in_channel", "1'x0");("out_channel", "1'x0");("state", "3'x0")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

    test "state_machine step7 2" [("in_channel", "1'x1");("out_channel", "1'x0");("state", "3'x1")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

    test "state_machine step8" [("in_channel", "1'x0");("out_channel", "1'x0");("state", "3'x0")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

     test "state_machine step9" [("in_channel", "1'x1");("out_channel", "1'x0");("state", "3'x1")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

    test "state_machine step10" [("in_channel", "1'x1");("out_channel", "1'x0");("state", "3'x1")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b0")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);

    test "state_machine step10" [("in_channel", "1'x1");("out_channel", "1'x0");("state", "3'x1")]
                    (sm_string
                    |> parse_circuit_no_errors
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> change_input "in_channel" (bitstream_of_binstring "1'b1")
                    |> step_n 2
                    |> register_values
                    |> StringMap.map (bitstream_to_hexstring)
                    |> StringMap.bindings);
]


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

    change_input_test "2 funcs"
        "register A[4] = 4'b0101 register B[4] = 4'b1001 fun f(x[4],y[4])[4] =
        x | y fun h(u[4],v[4])[4] = u && v register D[4] = f(A,B)
        register F[4] = h(B,A)" 3
                [("A", "4'x5");("B", "4'x9");("D", "4'xD");("F", "4'x1")]
                [];

    change_input_test "adder"
        "input A[1] input B[1] output S[1] = A ^ B output C[1] = A & B" 0
                [("A", "1'x1");("B", "1'x1");("C", "1'x1");("S", "1'x0")]
                [("A", (bitstream_of_binstring "1'b1"));
                ("B", (bitstream_of_binstring "1'b1"))];

] @ (test_state_machine inf_string) @ (test_state_machine opt_string)
