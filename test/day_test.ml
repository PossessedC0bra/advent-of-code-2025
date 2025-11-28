module type S = sig
  val tests : string * unit Alcotest.test_case list
end

module type T = sig
  module Day : Aoc2025.Day.S

  val part1_testable : Day.t1 Alcotest.testable
  val part1_test_data : (string * Day.t1) list
  val part2_testable : Day.t2 Alcotest.testable
  val part2_test_data : (string * Day.t2) list
end

module Make (Test : T) : S = struct
  let pool = Domainslib.Task.setup_pool ~num_domains:1 ()

  let tests =
    let make_tests name testable f =
      List.map (fun (input, expected) ->
        Alcotest.test_case name `Quick (fun () ->
          let actual = f input in
          Alcotest.check testable "must match" expected actual
        )
      )
    in
    let part1_test_cases =
      make_tests "Part 1" Test.part1_testable (Test.Day.part1 pool) Test.part1_test_data
    in
    let part2_test_cases =
      make_tests "Part 2" Test.part2_testable (Test.Day.part2 pool) Test.part2_test_data
    in
    ( Printf.sprintf "Day %02d - %s" Test.Day.day Test.Day.name
    , part1_test_cases @ part2_test_cases
    )
  ;;
end
