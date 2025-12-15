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

let example_input_2 =
  {|svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out|}
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
      {
        testable = Alcotest.int
      ; examples = [ (example_input_2, 2) ]
      ; constraints = None
      ; personal = Some 545_394_698_933_400
      }
    ;;

    let additional_tests = None
  end)
