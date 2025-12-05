type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 5
let name = "Cafeteria"

module Range = struct
  type t = {
      lower : int
    ; upper : int
  }

  let length r = r.upper + 1 - r.lower

  let make a b =
    let lower = min a b in
    let upper = max a b in
    { lower; upper }
  ;;

  let contains n r = r.lower <= n && n <= r.upper

  let overlap other r =
    contains other.lower r
    || contains other.upper r
    || contains r.lower other
    || contains r.upper other
  ;;

  let merge other r = { lower = min other.lower r.lower; upper = max other.upper r.upper }

  let compare r1 r2 =
    if r1.lower <> r2.lower then r1.lower - r2.lower else r1.upper - r2.upper
  ;;
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

let part2 _ s =
  let lines = s |> String.split_on_char '\n' in
  let separator_idx = lines |> List.find_index (( = ) "") |> Option.get in
  let ranges, _ =
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
  let r, rs =
    ranges
    |> List.sort Range.compare
    |> List.fold_left
         (fun acc r ->
            match acc with
            | None, rs -> (Some r, rs)
            | Some prev_r, rs ->
              if Range.overlap prev_r r
              then (Some (Range.merge prev_r r), rs)
              else (Some r, prev_r :: rs)
          )
         (None, [])
  in
  let merged_ranges = Option.get r :: rs in
  merged_ranges |> List.fold_left (fun acc r -> acc + Range.length r) 0
;;
