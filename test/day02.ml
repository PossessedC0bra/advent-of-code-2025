let test_input =
  {|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day02

    let part1_testable = Alcotest.int
    let part1_test_data = [ (test_input, 1_227_775_554) (* 20_223_751_480 *) ]
    let part2_testable = Alcotest.int
    let part2_test_data = [ (test_input, 4_174_379_265) (*30_260_171_216*) ]
  end)
