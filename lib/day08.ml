type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 8
let name = "Playground"

module Vec3d : sig
  type t = int * int * int

  val make : int -> int -> int -> t
  val x : t -> int
  val y : t -> int
  val z : t -> int
  val euclidian_distance : t -> t -> float
  val show : t -> string
end = struct
  type t = int * int * int

  let make x y z = (x, y, z)
  let x (x, _, _) = x
  let y (_, y, _) = y
  let z (_, _, z) = z

  let euclidian_distance (x0, y0, z0) (x1, y1, z1) =
    sqrt
      ((float_of_int (x0 - x1) ** 2.0)
       +. (float_of_int (y0 - y1) ** 2.0)
       +. (float_of_int (z0 - z1) ** 2.0)
      )
  ;;

  let show (x, y, z) = Printf.sprintf "(%d, %d, %d)" x y z
end

module DisjointSet : sig
  module Node : sig
    type t = {
        mutable parent : Vec3d.t
      ; mutable size : int
    }
  end

  type t

  val init : Vec3d.t list -> t
  val find : t -> Vec3d.t -> Node.t
  val union : t -> Vec3d.t -> Vec3d.t -> bool
  val to_seq_roots : t -> (Vec3d.t * Node.t) Seq.t
end = struct
  module Vec3dHashtbl = Hashtbl.Make (struct
      type t = Vec3d.t

      let equal = ( = )
      let hash = Hashtbl.hash
    end)

  module Node = struct
    type t = {
        mutable parent : Vec3d.t
      ; mutable size : int
    }
  end

  type t = Node.t Vec3dHashtbl.t

  let init ls =
    let number_of_elements = List.length ls in
    let tbl = Vec3dHashtbl.create number_of_elements in
    ls |> List.iter (fun p -> Vec3dHashtbl.add tbl p Node.{ parent = p; size = 1 });
    tbl
  ;;

  let rec find t p =
    let node = Vec3dHashtbl.find t p in
    if Node.(node.parent) <> p
    then (
      let root = find t Node.(node.parent) in
      Node.(node.parent <- root.parent);
      (* path compression *)
      root
    )
    else node
  ;;

  let union t p0 p1 =
    let root_0 = find t p0 in
    let root_1 = find t p1 in
    if root_0.parent = root_1.parent
    then false
    else if root_0.size >= root_1.size
    then (
      root_1.parent <- root_0.parent;
      root_0.size <- root_0.size + root_1.size;
      true
    )
    else (
      root_0.parent <- root_1.parent;
      root_1.size <- root_1.size + root_0.size;
      true
    )
  ;;

  let to_seq_roots t =
    t |> Vec3dHashtbl.to_seq |> Seq.filter (fun (p, node) -> Node.(p = node.parent))
  ;;
end

let parse_input s =
  s
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ',')
  |> List.map (function
    | [ x; y; z ] -> Vec3d.make (int_of_string x) (int_of_string y) (int_of_string z)
    | _ -> failwith "failed to parse vector with number of components not equal to 3"
    )
;;

let part1' number_of_connections _ s =
  let connect_cirquits n ps =
    let sets = DisjointSet.init ps in
    let all_connections =
      ps
      |> List.concat_map (fun p1 ->
        ps
        |> List.filter_map (fun p2 ->
          (* the graph is undirected -> only generate connection between two points once *)
          if p1 < p2 then Some (p1, p2, Vec3d.euclidian_distance p1 p2) else None
        )
      )
    in
    all_connections
    |> List.sort (fun (_, _, d1) (_, _, d2) -> Float.compare d1 d2)
    |> List.take n
    |> List.iter (fun (p1, p2, _) -> DisjointSet.union sets p1 p2 |> ignore);
    sets
  in
  s
  |> parse_input
  |> connect_cirquits number_of_connections
  |> DisjointSet.to_seq_roots
  |> List.of_seq
  |> List.sort
       (fun
           (_, DisjointSet.Node.{ size = s1; _ })
            (_, DisjointSet.Node.{ size = s2; _ }) (* sort from largest -> smallest *)
          -> Stdlib.compare s2 s1
     )
  |> List.take 3
  |> List.fold_left (fun acc (_, DisjointSet.Node.{ size; _ }) -> acc * size) 1
;;

let part1 p s = part1' 1_000 p s
let part2 _ _ = failwith "not yet implemented"
