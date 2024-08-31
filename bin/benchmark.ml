let num_lists, list_len, num_sorters, verbose, d =
  let n = ref 3_000
  and l = ref 3_000
  and s = ref 1
  and v = ref false
  and d = ref 10 in
  Arg.(
    parse
      [
        ("-lists", Set_int n, "number of lists");
        ("-length", Set_int l, "length of each list");
        ("-sorters", Set_int s, "number of sorters");
        ("-d", Set_int d, "d parameter");
        ("-verbose", Set v, "print tuplespace after insert");
      ]
      ignore
      "Usage: benchmark [-lists <int>] [-length <int>] [-sorters <int>] [-d \
       <int>] [-verbose]");
  (!n, !l, !s, !v, !d)

module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type int_list = int list [@@deriving equal, hash]

  let string_of_int_list l =
    List.map string_of_int l |> String.concat "; " |> Format.sprintf "[%s]"

  type t = Unsorted | Sorted | Val of int_list [@nested ""]
  [@@deriving equal, hash, to_string]
end

open Linda.Make (Element)

let string_of_tuple t = List.map Element.to_string t |> String.concat " "
let sort_list = List.sort compare

let fill ts () =
  let gen_list n = List.init n (fun _ -> Random.int 1000) in
  for _ = 1 to num_lists do
    out ts [ Unsorted; Val (gen_list list_len) ]
  done;
  if verbose then
    Format.printf "Inserted %d lists, current space: %s\n%!" num_lists
      (string_of_tuplespace ts string_of_tuple);
  []

let run_sort ts =
  (* let generator = eval ts (fill ts) in *)
  let _ = fill ts () in

  let t = Sys.time () in

  let sorters =
    List.init num_sorters (fun _ ->
        eval ts (fun () ->
            let rec loop () =
              match inp ts [ Match Unsorted; Wildcard ] with
              | Some [ Unsorted; Val l ] ->
                  out ts [ Sorted; Val (sort_list l) ];
                  loop ()
              | Some _ -> failwith "Invalid tuple"
              | None -> []
            in
            loop ()))
  in

  (* List.iter (await ts) (generator :: sorters); *)
  List.iter (await ts) sorters;
  Format.printf "Elapsed time: %f\n%!" (Sys.time () -. t);
  assert (inp ts [ Match Unsorted; Wildcard ] = None);
  print_endline "\n\nAll lists sorted!\n\n";
  if verbose then
    Format.printf "Final space: %s\n%!"
      (string_of_tuplespace ts string_of_tuple)

let () =
  (* Initialize the random number generator *)
  Random.init 42;
  (* Create a new tuplespace *)
  let ts = create_tuplespace ~d () in

  (* Run the tuple space *)
  run ts (fun () -> run_sort ts);

  (* Destroy the tuplespace *)
  destroy_tuplespace ts
