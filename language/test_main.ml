open OUnit2

(* main testing file compiled when you run [make test] from the command line *)

let suite = "CAMLWare Test Suite" >:::
            Test_bitstream.tests @ Test_simulator.tests 

let _ = run_test_tt_main suite
