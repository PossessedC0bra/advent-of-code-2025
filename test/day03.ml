include Day_test.Make (struct
    module Day = Aoc2025.Day03

    (* 98 *)
    let input =
      {|987654321111111
811111111111119
234234234234278
818181911112111|}
    ;;

    let part1_testable = Alcotest.int
    let part1_test_data = [ (input, 357) ]
    let part2_testable = Alcotest.int
    let part2_test_data = []
  end)
