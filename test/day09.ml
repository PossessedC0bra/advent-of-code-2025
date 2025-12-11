let example_input =
  {|7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day09

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 50) ]
      ; constraints = None
      ; personal = Some 4_752_484_112
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      { testable = Alcotest.int; examples = []; constraints = None; personal = None }
    ;;

    let additional_tests = None
  end)
