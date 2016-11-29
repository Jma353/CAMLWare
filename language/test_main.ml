open OUnit2

let suite = "CAMLWare Test Suite" >:::
            Test_bitstream.tests

let _ = run_test_tt_main suite
