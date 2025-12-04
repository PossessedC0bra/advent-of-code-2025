let example_input =
  {|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day02

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 1_227_775_554) ]
      ; constraints = None
      ; personal = Some 20_223_751_480
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 4_174_379_265) ]
      ; constraints = None
      ; personal = Some 30_260_171_216
      }
    ;;

    let additional_tests = None
  end)
