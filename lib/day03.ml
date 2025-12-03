type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 3
let name = "Lobby"

let extract_largest_number s =
  (* Algorithm: find largest number from the left (LEFT MOST index) *)
  let _, (largest_char_idx, largest_char_from_left) =
    String.sub s 0 (String.length s - 1)
    |> String.fold_left
         (fun (idx, (largest_char_idx, largest_char)) -> function
            | char when largest_char < char -> (idx + 1, (idx, char))
            | _ -> (idx + 1, (largest_char_idx, largest_char))
            )
         (0, (0, '0'))
  in
  (* find the largest number from the right up until LARGEST_NUMBER_FROM_LEFT *)
  let _, largest_char_from_right =
    String.fold_right
      (fun char (idx, largest_char) ->
         if idx > largest_char_idx && largest_char < char
         then (idx - 1, char)
         else (idx - 1, largest_char)
       )
      s
      (String.length s - 1, '0')
  in
  let number_of_char c = int_of_char c - int_of_char '0' in
  (number_of_char largest_char_from_left * 10) + number_of_char largest_char_from_right
;;

let part1 _ s =
  s
  |> String.split_on_char '\n'
  |> List.map extract_largest_number
  |> List.fold_left ( + ) 0
;;

let part2 _ _ = failwith "not yet implemented"
