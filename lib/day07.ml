type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 7
let name = "Laboratories"

type coordinate = int * int [@@deriving show]

module CoordinateSet = Set.Make (struct
    type t = coordinate

    let compare (x0, y0) (x1, y1) =
      match Stdlib.compare x0 x1 with
      | 0 -> Stdlib.compare y0 y1
      | c -> c
    ;;
  end)

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let string_foldi_left f acc l =
  l |> String.fold_left (fun (i, acc') c -> (i + 1, f i acc' c)) (0, acc) |> snd
;;

let foldi_left f acc l =
  l |> List.fold_left (fun (i, acc') e -> (i + 1, f i acc' e)) (0, acc) |> snd
;;

let parse_input s : coordinate option * CoordinateSet.t =
  s
  |> String.split_on_char '\n'
  |> foldi_left
       (fun y acc line ->
          line
          |> string_foldi_left
               (fun x ((start, splitters) as acc') -> function
                  | 'S' -> (Some (x, y), splitters)
                  | '^' -> (start, CoordinateSet.add (x, y) splitters)
                  | _ -> acc'
                  )
               acc
        )
       (None, CoordinateSet.empty)
;;

let part1 _ s =
  let start, splitters = s |> parse_input in
  let splitters_by_y =
    CoordinateSet.fold
      (fun (x, y) acc ->
         IntMap.update
           y
           (function
             | None -> Some ((x, y) :: [])
             | Some s -> Some ((x, y) :: s)
             )
           acc
       )
      splitters
      IntMap.empty
  in
  let max_y = IntMap.fold (fun y _ acc -> max y acc) splitters_by_y 0 in
  start
  |> Option.map (fun (start_x, start_y) ->
    let rec trace_rays y active_rays found =
      if y > max_y || IntSet.is_empty active_rays
      then found
      else (
        match IntMap.find_opt y splitters_by_y with
        | None -> trace_rays (y + 1) active_rays found
        | Some splitters_at_y ->
          let hits =
            List.filter (fun (sx, _) -> IntSet.mem sx active_rays) splitters_at_y
          in
          if List.is_empty hits
          then trace_rays (y + 1) active_rays found
          else (
            let hit_xs =
              List.fold_left (fun acc (x, _) -> IntSet.add x acc) IntSet.empty hits
            in
            let remaining_rays = IntSet.diff active_rays hit_xs in
            let new_rays =
              List.fold_left
                (fun acc (x, _) -> IntSet.add (x - 1) (IntSet.add (x + 1) acc))
                IntSet.empty
                hits
            in
            let active_rays' = IntSet.union remaining_rays new_rays in
            let found' =
              hits |> List.fold_left (fun set h -> CoordinateSet.add h set) found
            in
            trace_rays (y + 1) active_rays' found'
          )
      )
    in
    let reached_splitters =
      trace_rays (start_y + 1) (IntSet.singleton start_x) CoordinateSet.empty
    in
    reached_splitters |> CoordinateSet.cardinal
  )
  |> function
  | Some x -> x
  | None -> 0
;;

let part2 _ s =
  let start, splitters = s |> parse_input in
  let splitters_by_y =
    CoordinateSet.fold
      (fun (x, y) acc ->
         IntMap.update
           y
           (function
             | None -> Some ((x, y) :: [])
             | Some s -> Some ((x, y) :: s)
             )
           acc
       )
      splitters
      IntMap.empty
  in
  let max_y = IntMap.fold (fun y _ acc -> max y acc) splitters_by_y 0 in
  start
  |> Option.map (fun (start_x, start_y) ->
    let rec trace_rays y active_rays =
      if y > max_y || IntMap.is_empty active_rays
      then IntMap.fold (fun _ count acc -> acc + count) active_rays 0
      else (
        match IntMap.find_opt y splitters_by_y with
        | None -> trace_rays (y + 1) active_rays
        | Some splitters_at_y ->
          let hits =
            List.filter (fun (sx, _) -> IntMap.mem sx active_rays) splitters_at_y
          in
          if List.is_empty hits
          then trace_rays (y + 1) active_rays
          else (
            let active_rays' =
              List.fold_left
                (fun acc (x, _) ->
                   (* remember number of paths that existed until current splitter *)
                   let count = IntMap.find x active_rays in
                   acc
                   (* remove current ray *)
                   |> IntMap.remove x
                   (* split into left and right ray (summing up with any existing number of rays) *)
                   |> IntMap.update (x - 1) (function
                     | None -> Some count
                     | Some c -> Some (c + count)
                     )
                   |> IntMap.update (x + 1) (function
                     | None -> Some count
                     | Some c -> Some (c + count)
                     )
                 )
                active_rays
                hits
            in
            trace_rays (y + 1) active_rays'
          )
      )
    in
    let initial_rays = IntMap.singleton start_x 1 in
    trace_rays (start_y + 1) initial_rays
  )
  |> function
  | Some x -> x
  | None -> 0
;;
