type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 11
let name = "Reactor"

let part1 _ s =
  let parse_input s =
    s
    |> String.split_on_char '\n'
    |> List.map (fun line ->
      match line |> String.split_on_char ':' with
      | [ node; connected_nodes ] -> (node, connected_nodes |> String.split_on_char ' ')
      | _ -> failwith "Invalid line format. Failed to extract node and connected nodes"
    )
  in
  let find_paths_from_to ~src ~target graph =
    let q = Queue.create () in
    let rec bfs q =
      if Queue.is_empty q
      then 0
      else (
        let current_node = Queue.pop q in
        if current_node = target
        then 1 + bfs q
        else (
          match List.assoc_opt current_node graph with
          | Some next_nodes ->
            next_nodes |> List.iter (Fun.flip Queue.push q);
            bfs q
          | None -> bfs q
        )
      )
    in
    Queue.add src q;
    bfs q
  in
  s |> parse_input |> find_paths_from_to ~src:"you" ~target:"out"
;;

let part2 _ _ = failwith "not yet implemented"
