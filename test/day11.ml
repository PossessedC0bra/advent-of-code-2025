let example_input =
  {|aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day11

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 5) ]
      ; constraints = None
      ; personal = Some 708
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      { testable = Alcotest.int; examples = []; constraints = None; personal = None }
    ;;

    let additional_tests = None
  end)
