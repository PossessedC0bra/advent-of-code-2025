let example_input =
  {|3-5
10-14
16-20
12-18

1
5
8
11
17
32|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day05

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 3) ]
      ; constraints = None
      ; personal = Some 707
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 14) ]
      ; constraints = None
      ; personal = Some 361_615_643_045_059
      }
    ;;

    let additional_tests = None
  end)
