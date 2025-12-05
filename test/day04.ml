let example_input =
  {|..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day04

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 13) ]
      ; constraints = Some [ GREATER_THAN 1403 ]
      ; personal = Some 1491
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 43) ]
      ; constraints = None
      ; personal = Some 8722
      }
    ;;

    let additional_tests = None
  end)
