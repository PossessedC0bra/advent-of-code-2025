type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 3
let name = "Lobby"

let extract_largest_number ?(num_digits = 2) s =
  let range ?(from = 0) until = List.init (until - from) (fun i -> from + i) in
  let indicies = range num_digits in
  (* Algorithm: *)
  (* find the largest number from the left between [0 and s.len - (num_digits - idx)[ *)
  (* in the next iteration look for the largest number between prev_idx and s.len - (num_digits - idx) *)
  (* Complexity O(m*n) where m is num_digits and n is s.len *)
  indicies
  |> List.fold_left
       (fun (prev_char_idx, res) digit_idx ->
          let len = String.length s in
          let chars =
            String.sub
              s
              (prev_char_idx + 1)
              (len - (num_digits - digit_idx) - prev_char_idx)
          in
          let digit_value c = int_of_char c - int_of_char '0' in
          let _, largest_digit_idx, digit =
            String.fold_left
              (fun (idx, largest_digit_idx, largest_digit) -> function
                 | digit when largest_digit < digit -> (idx + 1, idx, digit)
                 | _ -> (idx + 1, largest_digit_idx, largest_digit)
                 )
              (0, 0, '0')
              chars
          in
          (prev_char_idx + 1 + largest_digit_idx, (res * 10) + digit_value digit)
        )
       (-1, 0)
  |> snd
;;

let part1 _ s =
  s
  |> String.split_on_char '\n'
  |> List.map (extract_largest_number ~num_digits:2)
  |> List.fold_left ( + ) 0
;;

let part2 _ s =
  s
  |> String.split_on_char '\n'
  |> List.map (extract_largest_number ~num_digits:12)
  |> List.fold_left ( + ) 0
;;
