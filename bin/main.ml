let days : Aoc2025.Day.t list =
  [
    (module Aoc2025.Day01)
  ; (module Aoc2025.Day02)
  ; (module Aoc2025.Day03)
  ; (module Aoc2025.Day04)
  ; (module Aoc2025.Day05)
  ; (module Aoc2025.Day06)
  ; (module Aoc2025.Day07)
  ; (module Aoc2025.Day08)
  ; (module Aoc2025.Day09)
  ; (module Aoc2025.Day10)
  ]
;;

(** Pretty-print duration from nanoseconds in compound format (e.g., "1m 23s 456ms")
    Below milliseconds, only one unit is shown (ms, µs, or ns) *)
let pp_duration_ns ns =
  let ns_per_us = 1_000L in
  let ns_per_ms = 1_000_000L in
  let ns_per_s = 1_000_000_000L in
  let ns_per_m = Int64.mul 60L ns_per_s in
  let parts = ref [] in
  let remaining = ref ns in
  (* minutes *)
  if !remaining >= ns_per_m
  then (
    let m = Int64.div !remaining ns_per_m in
    parts := Printf.sprintf "%Ldm" m :: !parts;
    remaining := Int64.rem !remaining ns_per_m
  );
  (* seconds *)
  if !remaining >= ns_per_s
  then (
    let s = Int64.div !remaining ns_per_s in
    parts := Printf.sprintf "%Lds" s :: !parts;
    remaining := Int64.rem !remaining ns_per_s
  );
  (* For sub-second precision, only show one unit: ms, µs, or ns *)
  if !remaining >= ns_per_ms
  then (
    let ms = Int64.div !remaining ns_per_ms in
    parts := Printf.sprintf "%Ldms" ms :: !parts
  )
  else if !remaining >= ns_per_us
  then (
    let us = Int64.div !remaining ns_per_us in
    parts := Printf.sprintf "%Ldµs" us :: !parts
  )
  else if !remaining > 0L || !parts = []
  then parts := Printf.sprintf "%Ldns" !remaining :: !parts;
  String.concat " " (List.rev !parts)
;;

let () =
  let pool =
    Domainslib.Task.setup_pool ~num_domains:(Domain.recommended_domain_count ()) ()
  in
  Domainslib.Task.run pool (fun () ->
    let day_promises =
      List.map
        (fun (module Day : Aoc2025.Day.S) ->
           let run =
             let ( let* ) = Result.bind in
             let* input =
               try
                 In_channel.with_open_text
                   (Printf.sprintf "input/day_%02d.txt" Day.day)
                   (fun ic -> Ok (In_channel.input_all ic)
                 )
               with
               | e -> Error (Printexc.to_string e)
             in
             let start pool part show =
               let measure_execution_time f =
                 let start_time = Mtime_clock.elapsed_ns () in
                 let result = f () in
                 let elapsed_ns = Int64.sub (Mtime_clock.elapsed_ns ()) start_time in
                 (elapsed_ns, result)
               in
               let map_snd = fun f (a, b) -> (a, f b) in
               Domainslib.Task.async pool (fun () ->
                 measure_execution_time part |> map_snd show
               )
             in
             Ok
               ( start pool (fun () -> Day.part1 pool input) Day.show_t1
               , start pool (fun () -> Day.part2 pool input) Day.show_t2
               )
           in
           ((module Day : Aoc2025.Day.S), run)
         )
        days
    in
    List.iter
      (fun ((module Day : Aoc2025.Day.S), res) ->
         Printf.printf "Day %02d - %s:\n" Day.day Day.name;
         (* flush to ensure puzzle title is visible immediately *)
         flush stdout;
         match res with
         | Ok (part1_promise, part2_promise) ->
           let handle_promise pool promise prefix =
             let result =
               try Ok (Domainslib.Task.await pool promise) with
               | e -> Error (Printexc.to_string e)
             in
             ( match result with
               | Ok (elapsed_ns, result) ->
                 Printf.printf "\t%s: %s (%s)\n" prefix result (pp_duration_ns elapsed_ns)
               | Error e -> Printf.printf "\t%s: ERROR: %s\n" prefix e
             );
             (* flush to ensure calculation is visible immediately upon completion. feels a lot more responsive *)
             flush stdout
           in
           handle_promise pool part1_promise "Part 1";
           handle_promise pool part2_promise "Part 2";
           Printf.printf "\n"
         | Error e ->
           Printf.printf "ERROR: %s\n\n" e;
           flush stdout
       )
      day_promises
  );
  Domainslib.Task.teardown_pool pool
;;
