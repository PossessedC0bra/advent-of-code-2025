type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 11
let name = "Reactor"

module StringMap = Hashtbl.Make (String)

let parse_input s =
  let map = Hashtbl.create 8 in
  s
  |> String.split_on_char '\n'
  |> List.map (fun line ->
    match line |> String.split_on_char ':' with
    | [ node; connected_nodes ] -> (node, connected_nodes |> String.split_on_char ' ')
    | _ -> failwith "Invalid line format. Failed to extract node and connected nodes"
  )
  |> List.iter (fun (node, connected_nodes) -> Hashtbl.add map node connected_nodes);
  map
;;

let find_paths_from_to ~src ~target graph =
  let dfs =
    let memo = Hashtbl.create 8 in
    let rec loop current_node =
      match Hashtbl.find_opt memo current_node with
      | Some m -> m
      | None ->
        if current_node = target
        then 1
        else (
          let number_of_paths =
            match Hashtbl.find_opt graph current_node with
            | Some next_nodes -> next_nodes |> List.map loop |> List.fold_left ( + ) 0
            | None -> 0
          in
          Hashtbl.replace memo current_node number_of_paths;
          number_of_paths
        )
    in
    loop src
  in
  dfs
;;

let part1 _ s = s |> parse_input |> find_paths_from_to ~src:"you" ~target:"out"

let part2 _ s =
  let graph = s |> parse_input in
  let number_of_paths_to_fft = find_paths_from_to ~src:"svr" ~target:"fft" graph in
  let number_of_paths_from_fft_to_dac =
    find_paths_from_to ~src:"fft" ~target:"dac" graph
  in
  let number_of_paths_from_dac_to_out =
    find_paths_from_to ~src:"dac" ~target:"out" graph
  in
  number_of_paths_to_fft * number_of_paths_from_fft_to_dac * number_of_paths_from_dac_to_out
;;
