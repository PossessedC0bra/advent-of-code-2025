let example_input =
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

let example_input_2 =
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

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 3) ]
      ; constraints = None
      ; personal = Some 1129
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 6); (example_input_2, 8) ]
      ; constraints = None
      ; personal = Some 6638
      }
    ;;

    let additional_tests = None
  end)
