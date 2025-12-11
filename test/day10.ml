let example_input =
  {|[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day10

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 7) ]
      ; constraints = None
      ; personal = None
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      { testable = Alcotest.int; examples = []; constraints = None; personal = None }
    ;;

    let additional_tests = None
  end)
