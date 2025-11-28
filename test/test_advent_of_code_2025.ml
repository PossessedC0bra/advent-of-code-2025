let days : (module Day_test.S) list = []

let () =
  let all_tests = List.map (fun (module D : Day_test.S) -> D.tests) days in
  Alcotest.run "Advent of Code 2025" all_tests
;;
