type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 1
let name = "Secret Entrance"

(* Constants *)
let start_position = 50
let track_length = 100

module Direction = struct
  type t =
    | L
    | R

  let parse = function
    | 'L' -> L
    | 'R' -> R
    | _ -> raise Not_found
  ;;

  let show = function
    | L -> "L"
    | R -> "R"
  ;;
end

let parse_input s : (Direction.t * int) list =
  let parse s =
    let length = String.length s in
    let direction = Direction.parse s.[0] in
    let amount = int_of_string (String.sub s 1 (length - 1)) in
    (direction, amount)
  in
  s |> String.split_on_char '\n' |> List.map parse
;;

let part1 _ s =
  s
  |> parse_input
  |> List.fold_left_map
       (fun old_position (dir, num) ->
          let new_position =
            match dir with
            | Direction.L -> old_position - num
            | Direction.R -> old_position + num
          in
          let new_position_wrapped = (new_position + track_length) mod track_length in
          (new_position_wrapped, new_position_wrapped = 0)
        )
       start_position
  |> snd
  |> List.filter Fun.id
  |> List.length
;;

let part2 _ s =
  s
  |> parse_input
  |> List.fold_left_map
       (fun old_position (dir, num) ->
          let full_rounds = num / track_length in
          let remaining_steps = num mod track_length in
          let new_position =
            match dir with
            | Direction.L -> old_position - remaining_steps
            | Direction.R -> old_position + remaining_steps
          in
          let has_crossed_zero =
            let is_past_boundary =
              match dir with
              | Direction.L -> new_position <= 0
              | Direction.R -> new_position >= track_length
            in
            (* avoid overcounting if we STARTED on zero *)
            old_position <> 0 && is_past_boundary
          in
          let times_passed_zero = full_rounds + Bool.to_int has_crossed_zero in
          let new_position_wrapped = (new_position + track_length) mod track_length in
          (new_position_wrapped, times_passed_zero)
        )
       start_position
  |> snd
  |> List.fold_left ( + ) 0
;;
