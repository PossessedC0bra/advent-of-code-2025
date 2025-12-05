type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 5
let name = "Cafeteria"

module Range = struct
  type t = {
      lower : int
    ; upper : int
  }

  let make lower_incl upper_incl = { lower = lower_incl; upper = upper_incl }
  let contains n r = r.lower <= n && n <= r.upper
end

let part1 _ s =
  let lines = s |> String.split_on_char '\n' in
  let separator_idx = lines |> List.find_index (( = ) "") |> Option.get in
  let ranges, product_ids =
    lines
    |> List.fold_left
         (fun (idx, (l, r)) elem ->
            match idx with
            | i when i < separator_idx -> (idx + 1, (elem :: l, r))
            | i when i > separator_idx -> (idx + 1, (l, elem :: r))
            | _ -> (idx + 1, (l, r))
          )
         (0, ([], []))
    |> snd
  in
  let ranges =
    ranges
    |> List.map (fun s ->
      s
      |> String.split_on_char '-'
      |> function
      | [ l; h ] -> Range.make (int_of_string l) (int_of_string h)
      | _ -> failwith "invalid range format"
    )
  in
  product_ids
  |> List.map int_of_string
  |> List.filter (fun id -> ranges |> List.exists (fun r -> Range.contains id r))
  |> List.length
;;

let part2 _ _ = failwith "not yet implemented"
