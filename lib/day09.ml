type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 9
let name = "Movie Theater"

let part1 _ s =
  let points =
    s
    |> String.split_on_char '\n'
    |> List.map (fun line ->
      line
      |> String.split_on_char ','
      |> function
      | [ x; y ] -> (int_of_string x, int_of_string y)
      | _ -> failwith "failed to parse position with less / more components than 2"
    )
  in
  points
  |> List.fold_left
       (fun acc (x1, y1) ->
          points
          |> List.fold_left
               (fun acc (x2, y2) ->
                  let width = abs (x1 - x2) + 1 in
                  let height = abs (y1 - y2) + 1 in
                  let area = width * height in
                  if area > acc
                  then (

                    area
                  )
                  else acc
                )
               acc
        )
       0
;;

let part2 _ _ = failwith "not yet implemented"
