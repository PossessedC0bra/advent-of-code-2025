let example_input =
  {|162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689|}
;;

include Day_test.Make (struct
    module Day = Aoc2025.Day08

    let part1 : Day.t1 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [] (* see additional tests *)
      ; constraints = Some [ NOT_EQUAL 93; GREATER_THAN 29696; GREATER_THAN 512 ]
      ; personal = Some 66_640
      }
    ;;

    let part2 : Day.t2 Day_test.part_spec =
      {
        testable = Alcotest.int
      ; examples = [ (example_input, 25_272) ]
      ; constraints = None
      ; personal = Some 78_894_156
      }
    ;;

    let additional_tests =
      let pool = Domainslib.Task.setup_pool ~num_domains:1 () in
      Some
        [
          Alcotest.test_case "Part 1 - Example (10 connections)" `Quick (fun () ->
            Alcotest.(check int) "must be equal" 40 (Day.part1' 10 pool example_input)
          )
        ]
    ;;
  end)
