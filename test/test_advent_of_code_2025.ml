let day_tests : (module Day_test.S) list =
  [
    (module Day01)
  ; (module Day02)
  ; (module Day03)
  ; (module Day04)
  ; (module Day05)
  ; (module Day06)
  ; (module Day07)
  ; (module Day08)
  ; (module Day09)
  ; (module Day10)
  ; (module Day11)
  ]
;;

let () =
  let day_suites =
    day_tests |> List.map (fun (module D : Day_test.S) -> (D.name, D.tests))
  in
  Alcotest.run "Advent of Code 2025" day_suites
;;
