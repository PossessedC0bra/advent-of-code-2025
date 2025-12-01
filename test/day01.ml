let test_input =
  {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82|}
;;

let test_input_2 =
  {|L68
L30
R48
L5
R160
L55
L1
L199
R14
L82|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day01

    let part1_testable = Alcotest.int
    let part1_test_data = [ (test_input, 3) (*1129*)]
    let part2_testable = Alcotest.int
    let part2_test_data = [ (test_input, 6); (test_input_2, 8) (*6638*) ]
  end)
