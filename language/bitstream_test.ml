open OUnit2
open Bitstream

let six = create [false; true; true; false; false;]
let five = create [true; false; true; false;]
let eleven = create [true; true; false; true; false]
let o = create [true; false; false; false; false;]

let tests = "test suite" >::: [
  "add"  >:: (fun _ -> assert_equal (add six five) eleven );
  "subtract"  >:: (fun _ -> assert_equal (subtract six five) o );
  "add_test 0" >:: (fun _ -> assert_equal

  (decimal_of_bitstream
  	((add 	(create
  			[true; ] )
  		(create
  			[true; false; true; false; false; true; ] )))) 38);
]

let _ = run_test_tt_main tests
