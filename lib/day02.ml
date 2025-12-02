type t1 = int [@@deriving show]
type t2 = int [@@deriving show]

let day = 2
let name = "Gift Shop"

let parse_input s =
  s
  |> String.split_on_char ','
  |> List.map (String.split_on_char '-')
  |> List.map (List.map int_of_string)
  |> List.map (function
    | [ a; b ] -> (a, b)
    | _ ->
      raise
        (Invalid_argument "Product ID ranges must have exactly two numbers: start and end")
    )
;;

let part1 _ s =
  let generate_symmetrical_numbers_between s e =
    let nums = List.init (e - s + 1) (( + ) s) in
    let is_symmetrical s =
      let len = String.length s in
      len mod 2 = 0 && String.sub s 0 (len / 2) = String.sub s (len / 2) (len / 2)
    in
    nums |> List.filter (fun n -> n |> string_of_int |> is_symmetrical)
  in
  s
  |> parse_input
  |> List.concat_map (fun (s, e) -> generate_symmetrical_numbers_between s e)
  |> List.fold_left ( + ) 0
;;

let part2 _ s =
  let generate_number_with_repeating_digits s e =
    let nums = List.init (e - s + 1) (( + ) s) in
    (* compile regex outside of function that is called inside loop to avoid frequent recompilation *)
    (* use study true to profit from possible runtime optimizations *)
    (* due to the use of backreferences i have to use the PCRE library rather than the RE library *)
    let symmetry_regex = Pcre.regexp ~study:true {|^(\d+?)\1+$|} in
    let contains_symmetrical_patterns s = Pcre.pmatch ~rex:symmetry_regex s in
    nums |> List.filter (fun s -> s |> string_of_int |> contains_symmetrical_patterns)
  in
  s
  |> parse_input
  |> List.concat_map (fun (s, e) -> generate_number_with_repeating_digits s e)
  |> List.fold_left ( + ) 0
;;
