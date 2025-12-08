type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 6
let name = "Trash Compactor"

let parse_input s =
  s
  |> String.split_on_char '\n'
  |> List.map (fun s -> s |> String.split_on_char ' ' |> List.filter (fun s -> s <> ""))
;;

type operator =
  | ADDITION
  | MUTLIPLICATION

let part1 _ s =
  let input = parse_input s in
  input
  |> List.rev
  |> List.fold_left
       (fun (idx, (results, operators)) line ->
          if idx = 0
          then (
            let operators =
              line
              |> List.map (function
                | "+" -> ADDITION
                | "*" -> MUTLIPLICATION
                | x -> failwith ("invalid operator: " ^ x)
                )
            in
            let results =
              operators
              |> List.map (function
                | ADDITION -> 0
                | MUTLIPLICATION -> 1
                )
            in
            (idx + 1, (results, operators))
          )
          else
            ( idx + 1
            , let operands = List.combine results line in
              ( List.map2
                  (fun (res, s) -> function
                     | ADDITION -> res + int_of_string s
                     | MUTLIPLICATION -> res * int_of_string s
                     )
                  operands
                  operators
              , operators
              )
            )
        )
       (0, ([], []))
  |> snd
  |> fst
  |> List.fold_left ( + ) 0
;;

let part2 _ _ = failwith "not yet implemented"
