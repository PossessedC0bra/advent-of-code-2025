type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

module Board = struct
  type t = {
      width : int
    ; height : int
    ; board : Buffer.t
  }

  let make s =
    let rows = s |> String.split_on_char '\n' in
    let height = List.length rows in
    let width = String.length (List.hd rows) in
    let board =
      let buf = Buffer.create (width * height) in
      List.iter (fun row -> Buffer.add_string buf row) rows;
      buf
    in
    { width; height; board }
  ;;

  let calculate_index w h t = (h * t.width) + w

  let get w h t =
    assert (w >= 0 && w < t.width);
    assert (h >= 0 && h < t.height);
    Buffer.nth t.board (calculate_index w h t)
  ;;

  let get_opt w h t =
    try Some (get w h t) with
    | _ -> None
  ;;

  let fold f acc t =
    let height_idxs = List.init t.height Fun.id in
    let width_idxs = List.init t.width Fun.id in
    height_idxs
    |> List.fold_left
         (fun acc h -> width_idxs |> List.fold_left (fun acc w -> f acc w h) acc)
         acc
  ;;
end

let day = 4
let name = "Printing Department"

let part1 _ s =
  let board = Board.make s in
  board
  |> Board.fold
       (fun acc w h ->
          match (w, h) with
          | w, h when Board.get w h board <> '@' -> acc
          | w, h ->
            let tl = Board.get_opt (w - 1) (h - 1) board in
            let tm = Board.get_opt w (h - 1) board in
            let rl = Board.get_opt (w + 1) (h - 1) board in
            let ml = Board.get_opt (w - 1) h board in
            let mr = Board.get_opt (w + 1) h board in
            let bl = Board.get_opt (w - 1) (h + 1) board in
            let bm = Board.get_opt w (h + 1) board in
            let br = Board.get_opt (w + 1) (h + 1) board in
            let number_of_adjacent_paper_rolls =
              [ tl; tm; rl; ml; mr; bl; bm; br ]
              |> List.filter_map Fun.id
              |> List.map (fun c -> c = '@')
              |> List.map Bool.to_int
              |> List.fold_left ( + ) 0
            in
            let is_free = number_of_adjacent_paper_rolls < 4 in
            acc + Bool.to_int is_free
        )
       0
;;

let part2 _ _ = failwith "not yet implemented"
