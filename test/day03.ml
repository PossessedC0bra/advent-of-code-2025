let example_input =
  {|987654321111111
811111111111119
234234234234278
818181911112111|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day03

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 357) ]
      ; constraints = None
      ; personal = Some 17_613
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 3_121_910_778_619) ]
      ; constraints = None
      ; personal = Some 175_304_218_462_560
      }
    ;;

    let additional_tests =
      Some
        [
          Alcotest.test_case
            "extract_largest_number - Line 1 (2 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 98
                 (Aoc2025.Day03.extract_largest_number "987654321111111")
          )
        ; Alcotest.test_case
            "extract_largest_number - Line 2 (2 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 89
                 (Aoc2025.Day03.extract_largest_number "811111111111119")
          )
        ; Alcotest.test_case
            "extract_largest_number - Line 3 (2 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 78
                 (Aoc2025.Day03.extract_largest_number "234234234234278")
          )
        ; Alcotest.test_case
            "extract_largest_number - Line 4 (2 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 92
                 (Aoc2025.Day03.extract_largest_number "818181911112111")
          )
        ; Alcotest.test_case
            "extract_largest_number - Line 1 (12 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 987654321111
                 (Aoc2025.Day03.extract_largest_number ~num_digits:12 "987654321111111")
          )
        ; Alcotest.test_case
            "extract_largest_number - Line 2 (12 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 811111111119
                 (Aoc2025.Day03.extract_largest_number ~num_digits:12 "811111111111119")
          )
        ; Alcotest.test_case
            "extract_largest_number - Line 3 (12 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 434234234278
                 (Aoc2025.Day03.extract_largest_number ~num_digits:12 "234234234234278")
          )
        ; Alcotest.test_case
            "extract_largest_number - Line 4 (12 digits)"
            `Quick
            (fun () ->
               Alcotest.(check int)
                 "must be equal"
                 888911112111
                 (Aoc2025.Day03.extract_largest_number ~num_digits:12 "818181911112111")
          )
        ]
    ;;
  end)
