let days : Aoc2025.Day.t list = [ (module Aoc2025.Day01) ]

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
                 let start_time = Sys.time () in
                 (Sys.time () -. start_time, f ())
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
               | Ok (time, result) -> Printf.printf "\t%s: %s (%fms)\n" prefix result time
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
