type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 1
let name = "Secret Entrance"

type direction =
  | L
  | R
[@@deriving show]

let parse_direction s : direction =
  match s with
  | 'L' -> L
  | 'R' -> R
  | _ -> raise Not_found
;;

let show_direction d =
  match d with
  | L -> "L"
  | R -> "R"
;;

let parse_input s : (direction * int) list =
  let parse s =
    let length = String.length s in
    let direction = parse_direction s.[0] in
    let amount = int_of_string (String.sub s 1 (length - 1)) in
    (direction, amount)
  in
  s |> String.split_on_char '\n' |> List.map parse
;;

let part1 _ s =
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

let part2 _ s =
  s
  |> parse_input
  |> List.fold_left_map
       (fun acc (dir, num) ->
          let x =
            match dir with
            | L -> acc - num
            | R -> acc + num
          in
          let shifted_x = (x + 100) mod 100 in
          let num_of_times_past_zero =
            let y = abs (x / 100) in
            match dir with
            | L -> (if acc > 0 && (shifted_x = 0 || shifted_x > x) then 1 else 0) + y
            | R -> y
          in
          let new_acc = (shifted_x + 100) mod 100 in
          (new_acc, num_of_times_past_zero)
        )
       50
  |> snd
  |> List.fold_left ( + ) 0
;;
