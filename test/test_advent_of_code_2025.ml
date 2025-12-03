let days : (module Day_test.S) list = [ (module Day01); (module Day02); (module Day03) ]

let custom_tests =
  [
    ( "Day 03 - CUSTOM"
    , [
        Alcotest.test_case "Test Input - Line 1" `Quick (fun () ->
          Alcotest.(check int)
            "must be equal"
            98
            (Aoc2025.Day03.extract_largest_number "987654321111111")
        )
      ; Alcotest.test_case "Test Input - Line 2" `Quick (fun () ->
          Alcotest.(check int)
            "must be equal"
            89
            (Aoc2025.Day03.extract_largest_number "811111111111119")
        )
      ; Alcotest.test_case "Test Input - Line 3" `Quick (fun () ->
          Alcotest.(check int)
            "must be equal"
            78
            (Aoc2025.Day03.extract_largest_number "234234234234278")
        )
      ; Alcotest.test_case "Test Input - Line 4" `Quick (fun () ->
          Alcotest.(check int)
            "must be equal"
            92
            (Aoc2025.Day03.extract_largest_number "818181911112111")
        )
      ]
    )
  ; ("----", [ Alcotest.test_case "" `Quick (fun () -> ()) ])
  ]
;;

let () =
  let all_tests = List.map (fun (module D : Day_test.S) -> D.tests) days in
  Alcotest.run "Advent of Code 2025" (custom_tests @ all_tests)
;;
