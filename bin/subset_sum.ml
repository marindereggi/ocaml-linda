let num_lists, list_len, num_workers, verbose, d =
  let n = ref 1_000
  and l = ref 30
  and w = ref 1
  and v = ref false
  and d = ref 10 in
  Arg.(
    parse
      [
        ("-lists", Set_int n, "number of lists");
        ("-length", Set_int l, "length of each list");
        ("-w", Set_int w, "number of workers");
        ("-verbose", Set v, "print tuplespace after insert");
        ("-d", Set_int d, "d parameter");
      ]
      ignore
      "Usage: subset_sum [-lists <int>] [-length <int>] [-w <int>] [-verbose] \
       [-d <int>]");
  (!n, !l, !w, !v, !d)

module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type int_list = int list [@@deriving equal, hash]

  let string_of_int_list l =
    List.map string_of_int l |> String.concat "; " |> Format.sprintf "[%s]"

  type t =
    | Subset_sum
    | Result
    | List of int_list [@nested ""]
    | Sum of int [@nested ""]
    | Solution of bool [@nested ""]
  [@@deriving equal, hash, to_string]
end

open Linda.Make (Element)

let string_of_tuple t = List.map Element.to_string t |> String.concat " "
let generate n = List.init n (fun _ -> Random.int 100 + 1)

let fill ts () =
  for _ = 1 to num_lists do
    out ts [ Subset_sum; List (generate list_len); Sum (Random.int 1000 + 1) ]
  done;
  if verbose then
    Format.printf "Inserted %d lists, current space: %s\n%!" num_lists
      (string_of_tuplespace ts string_of_tuple);
  []

let rec subset_sum numbers target =
  match numbers with
  | [] -> target = 0
  | x :: xs -> subset_sum xs target || subset_sum xs (target - x)

let subset_sum_problem ts =
  (* let generator = eval ts (fill ts) in *)
  let _ = fill ts () in

  (* let t = Sys.time () in *)
  let workers =
    List.init num_workers (fun _ ->
        eval ts (fun () ->
            let rec loop () =
              match inp ts [ Match Subset_sum; Wildcard; Wildcard ] with
              | Some [ Subset_sum; List l; Sum s ] ->
                  out ts [ Result; List l; Sum s; Solution (subset_sum l s) ];
                  loop ()
              | None -> []
              | _ -> assert false
            in
            loop ()))
  in

  (* List.iter (await ts) (generator :: sorters); *)
  List.iter (await ts) workers;
  (* Format.printf "Elapsed time: %f\n%!" (Sys.time () -. t); *)
  assert (inp ts [ Match Subset_sum; Wildcard; Wildcard ] = None);
  (* print_endline "\n\nAll lists partitioned!\n\n"; *)
  if verbose then
    Format.printf "Final space: %s\n%!"
      (string_of_tuplespace ts string_of_tuple)

let () =
  (* Initialize the random number generator *)
  Random.init 42;
  (* Create a new tuplespace *)
  let ts = create_tuplespace ~d () in
  (* Run the tuple space *)
  run ts (fun () -> subset_sum_problem ts);
  (* Destroy the tuplespace *)
  destroy_tuplespace ts
