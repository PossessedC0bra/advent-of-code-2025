module type S = sig
  val name : string
  val tests : unit Alcotest.test_case list
end

type 'a constraint_ =
  | LESS_THAN of 'a
  | NOT_EQUAL of 'a
  | GREATER_THAN of 'a

type 'a part_spec = {
    testable : 'a Alcotest.testable
  ; examples : (string * 'a) list
  ; constraints : 'a constraint_ list option
  ; personal : 'a option
}

module type T = sig
  module Day : Aoc2025.Day.S

  val part1 : Day.t1 part_spec
  val part2 : Day.t2 part_spec
  val additional_tests : unit Alcotest.test_case list option
end

module Make (Test : T) : S = struct
  let pool = Domainslib.Task.setup_pool ~num_domains:1 ()
  let name = Printf.sprintf "Day %02d - %s" Test.Day.day Test.Day.name

  let read_personal_input () =
    In_channel.with_open_text
      (Printf.sprintf "../input/day_%02d.txt" Test.Day.day)
      In_channel.input_all
  ;;

  let run_with_pool f input = Domainslib.Task.run pool (fun () -> f input)

  let make_example_tests part_name testable f examples =
    let test_name idx =
      if idx = 0
      then Printf.sprintf "%s - Example" part_name
      else Printf.sprintf "%s - Example %d" part_name (idx + 1)
    in
    List.mapi
      (fun i (input, expected) ->
         Alcotest.test_case (test_name i) `Quick (fun () ->
           let actual = run_with_pool f input in
           Alcotest.check testable "must match" expected actual
         )
       )
      examples
  ;;

  let make_constraint_test part_name show f = function
    | None -> []
    | Some constraints ->
      [
        Alcotest.test_case (Printf.sprintf "%s - Constraints" part_name) `Quick (fun () ->
          let input = read_personal_input () in
          let actual = run_with_pool f input in
          constraints
          |> List.iter (function
            | GREATER_THAN v ->
              if compare actual v <= 0
              then
                Alcotest.fail
                  (Printf.sprintf
                     "Result %s is not greater than %s (known too low)"
                     (show actual)
                     (show v)
                  )
            | LESS_THAN v ->
              if compare actual v >= 0
              then
                Alcotest.fail
                  (Printf.sprintf
                     "Result %s is not less than %s (known too high)"
                     (show actual)
                     (show v)
                  )
            | NOT_EQUAL v ->
              if compare actual v = 0
              then
                Alcotest.fail
                  (Printf.sprintf
                     "Result %s equals known wrong answer %s"
                     (show actual)
                     (show v)
                  )
            )
        )
      ]
  ;;

  let make_personal_test part_name testable f = function
    | None -> []
    | Some expected ->
      [
        Alcotest.test_case
          (Printf.sprintf "%s - Personal Input" part_name)
          `Quick
          (fun () ->
             let input = read_personal_input () in
             let actual = run_with_pool f input in
             Alcotest.check testable "must match" actual expected
        )
      ]
  ;;

  let tests =
    let part1_examples =
      make_example_tests
        "Part 1"
        Test.part1.testable
        (Test.Day.part1 pool)
        Test.part1.examples
    in
    let part1_constraints =
      make_constraint_test
        "Part 1"
        Test.Day.show_t1
        (Test.Day.part1 pool)
        Test.part1.constraints
    in
    let part1_personal =
      make_personal_test
        "Part 1"
        Test.part1.testable
        (Test.Day.part1 pool)
        Test.part1.personal
    in
    let part2_examples =
      make_example_tests
        "Part 2"
        Test.part2.testable
        (Test.Day.part2 pool)
        Test.part2.examples
    in
    let part2_constraints =
      make_constraint_test
        "Part 2"
        Test.Day.show_t2
        (Test.Day.part2 pool)
        Test.part2.constraints
    in
    let part2_personal =
      make_personal_test
        "Part 2"
        Test.part2.testable
        (Test.Day.part2 pool)
        Test.part2.personal
    in
    let additional_tests =
      match Test.additional_tests with
      | Some tests -> tests
      | None -> []
    in
    part1_examples
    @ part1_constraints
    @ part1_personal
    @ part2_examples
    @ part2_constraints
    @ part2_personal
    @ additional_tests
  ;;
end
