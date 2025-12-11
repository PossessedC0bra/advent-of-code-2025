type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 10
let name = "Factory"

let part1 _ s =
  let parse_input s =
    s
    |> String.split_on_char '\n'
    |> List.map (fun line ->
      let machine_parts = line |> String.split_on_char ' ' in
      let light_diagram =
        let string_foldi_left (f : int -> 'acc -> char -> 'acc) (acc : 'acc) (s : string) =
          s |> String.fold_left (fun (i, acc') c -> (i + 1, f i acc' c)) (0, acc) |> snd
        in
        let diagram = List.hd machine_parts in
        String.sub diagram 1 (String.length diagram - 2)
        |> string_foldi_left
             (fun i acc -> function
                | '#' -> acc lor (1 lsl i)
                | '.' -> acc
                | c -> failwith (Printf.sprintf "invalid character in light diagram: %c" c)
                )
             0
      in
      let wiring_schematics, joltage_requirements =
        let rec parse (wirings, joltages) =
          let parse_wiring s =
            let len = String.length s in
            let toggle_indicies =
              String.sub s 1 (len - 2)
              |> String.split_on_char ','
              |> List.map int_of_string
            in
            toggle_indicies
            |> List.fold_left (fun acc toggle_idx -> acc lor (1 lsl toggle_idx)) 0
          in
          let parse_joltage s =
            let len = String.length s in
            String.sub s 1 (len - 2) |> String.split_on_char ',' |> List.map int_of_string
          in
          function[@ocaml.warning "-8"]
          | joltage :: [] -> (wirings, parse_joltage joltage @ joltages)
          | wiring :: rest -> parse (parse_wiring wiring :: wirings, joltages) rest
        in
        parse ([], []) (List.drop 1 machine_parts)
      in
      (light_diagram, wiring_schematics, joltage_requirements)
    )
  in
  let shortest_enable_sequence_length expected wirings =
    let rec bfs q =
      if Queue.is_empty q
      then failwith "failed to find solution for wiring"
      else (
        let depth, diagram = Queue.pop q in
        if diagram = expected
        then depth
        else (
          wirings
          |> List.iter (fun w ->
            let new_diagram = diagram lxor w in
            Queue.add (depth + 1, new_diagram) q
          );
          bfs q
        )
      )
    in
    let q = Queue.create () in
    Queue.add (0, 0) q;
    bfs q
  in
  s
  |> parse_input
  |> List.map (fun (expected, wirings, _) ->
    shortest_enable_sequence_length expected wirings
  )
  |> List.fold_left ( + ) 0
;;

let part2 _ _ = failwith "not yet implemented"
