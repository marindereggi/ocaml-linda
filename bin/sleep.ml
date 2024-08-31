let num_sleeps, num_workers, verbose, d, sleep_time =
  let n = ref 240
  and w = ref 1
  and v = ref false
  and d = ref 10
  and s = ref 30. in
  Arg.(
    parse
      [
        ("-n", Set_int n, "number of sleeps");
        ("-w", Set_int w, "number of workers");
        ("-verbose", Set v, "print tuplespace after insert");
        ("-d", Set_int d, "d parameter");
        ("-s", Set_float s, "sleep time (in s)");
      ]
      ignore
      "Usage: sleep [-n <int>] [-w <int>] [-verbose] [-d <int>] [-s <float>]");
  (!n, !w, !v, !d, !s /. float_of_int !n)

module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type t = Sleep | Done | Float of float [@nested ""]
  [@@deriving equal, hash, to_string]
end

open Linda.Make (Element)

let string_of_tuple t = List.map Element.to_string t |> String.concat " "

let fill ts () =
  for _ = 1 to num_sleeps do
    out ts [ Sleep; Float sleep_time ]
  done;
  if verbose then
    Format.printf "Inserted %d sleeps, current space: %s\n%!" num_sleeps
      (string_of_tuplespace ts string_of_tuple);
  []

let rec sleep_worker ts =
  match inp ts [ Match Sleep; Wildcard ] with
  | Some [ Sleep; Float f ] ->
      Unix.sleepf f;
      out ts [ Done; Float f ];
      sleep_worker ts
  | None -> [ Element.Done ]
  | _ -> assert false

let sleeping_problem ts =
  (* let generator = eval ts (fill ts) in *)
  let _ = fill ts () in

  (* let t = Sys.time () in *)
  let workers =
    List.init (num_workers - 1) (fun _ -> eval ts (fun () -> sleep_worker ts))
  in

  out ts (sleep_worker ts);

  (* List.iter (await ts) (generator :: sorters); *)
  List.iter (await ts) workers;
  (* Format.printf "Elapsed time: %f\n%!" (Sys.time () -. t); *)
  assert (inp ts [ Match Sleep; Wildcard ] = None);
  if verbose then
    Format.printf "Final space: %s\n%!"
      (string_of_tuplespace ts string_of_tuple)

let () =
  (* Initialize the random number generator *)
  Random.init 42;
  (* Create a new tuplespace *)
  let ts = create_tuplespace ~d () in
  (* Run the tuple space *)
  run ts (fun () -> sleeping_problem ts);
  (* Destroy the tuplespace *)
  destroy_tuplespace ts
