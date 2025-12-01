type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 1
let name = "Secret Entrance"

type direction =
  | L
  | R

let parse_direction s : direction =
  match s with
  | 'L' -> L
  | 'R' -> R
  | _ -> raise Not_found
;;

let part1 _ s =
  let parse_input s : (direction * int) list =
    let parse s =
      let length = String.length s in
      let direction = parse_direction s.[0] in
      let amount = int_of_string (String.sub s 1 (length - 1)) in
      (direction, amount)
    in
    s |> String.split_on_char '\n' |> List.map parse
  in
  s
  |> parse_input
  |> List.fold_left_map
       (fun acc (dir, num) ->
          let x =
            match dir with
            | L -> acc - num
            | R -> acc + num
          in
          let y = (x + 100) mod 100 in
          (y, y = 0)
        )
       50
  |> snd
  |> List.filter Fun.id
  |> List.length
;;

let part2 _ _ = failwith "not yet implemented"
