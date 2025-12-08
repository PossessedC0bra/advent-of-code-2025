type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 6
let name = "Trash Compactor"

type operator =
  | ADDITION
  | MUTLIPLICATION

let parse_input s : (string list * operator) list =
  let lines = s |> String.split_on_char '\n' in
  let number_of_lines = List.length lines in
  let line_length = lines |> List.hd |> String.length in
  let flush_buffers bs =
    let strings =
      bs
      |> Array.map (fun buf ->
        let res = Buffer.contents buf in
        Buffer.clear buf;
        res
      )
    in
    let operands = Array.sub strings 0 (number_of_lines - 1) in
    let op =
      Array.get strings (number_of_lines - 1)
      |> String.trim
      |> function
      | "+" -> ADDITION
      | "*" -> MUTLIPLICATION
      | x -> failwith ("invalid operator: " ^ x)
    in
    (operands |> Array.to_list, op)
  in
  let buffers, problems =
    List.init line_length Fun.id
    |> List.fold_left
         (fun (buffers, problems) idx ->
            let chars = lines |> List.map (fun line -> String.get line idx) in
            let is_spacer_row = chars |> List.for_all (( = ) ' ') in
            if is_spacer_row
            then (buffers, flush_buffers buffers :: problems)
            else (
              chars
              |> List.iteri (fun idx c ->
                let buf = Array.get buffers idx in
                Buffer.add_char buf c
              );
              (buffers, problems)
            )
          )
         (Array.init number_of_lines (fun _ -> Buffer.create 8), [])
  in
  flush_buffers buffers :: problems
;;

let part1 _ s =
  s
  |> parse_input
  |> List.fold_left
       (fun acc (operands, op) ->
          let calculate = function
            | ADDITION -> List.fold_left ( + ) 0
            | MUTLIPLICATION -> List.fold_left ( * ) 1
          in
          let result =
            operands
            |> List.map (fun s -> s |> String.trim |> int_of_string)
            |> calculate op
          in
          result + acc
        )
       0
;;

let part2 _ s =
  let string_of_chars cs =
    let buf = Buffer.create (List.length cs) in
    cs |> List.iter (Buffer.add_char buf);
    Buffer.contents buf
  in
  let transpose strings =
    let rec aux acc = function
      | strings when List.for_all (fun s -> s = "") strings -> List.rev acc
      | strings ->
        let heads = strings |> List.map (fun s -> s.[0]) in
        let tails = strings |> List.map (fun s -> String.sub s 1 (String.length s - 1)) in
        aux (heads :: acc) tails
    in
    aux [] strings
  in
  s
  |> parse_input
  |> List.map (fun (operands, op) -> (transpose operands |> List.map string_of_chars, op))
  |> List.fold_left
       (fun acc (operands, op) ->
          let calculate = function
            | ADDITION -> List.fold_left ( + ) 0
            | MUTLIPLICATION -> List.fold_left ( * ) 1
          in
          let result =
            operands
            |> List.map (fun s -> s |> String.trim |> int_of_string)
            |> calculate op
          in
          result + acc
        )
       0
;;
