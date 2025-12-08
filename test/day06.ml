let example_input =
  {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   + |}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day06

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 4_277_556) ]
      ; constraints = None
      ; personal = Some 5_524_274_308_182
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      { testable = Alcotest.int; examples = []; constraints = None; personal = None }
    ;;

    let additional_tests = None
  end)
