let num_lists, list_len, num_partitioners, verbose, d =
  let n = ref 10_000
  and l = ref 50
  and p = ref 1
  and v = ref false
  and d = ref 10 in
  Arg.(
    parse
      [
        ("-lists", Set_int n, "number of lists");
        ("-length", Set_int l, "length of each list");
        ("-p", Set_int p, "number of partitioners");
        ("-verbose", Set v, "print tuplespace after insert");
        ("-d", Set_int d, "d parameter");
      ]
      ignore
      "Usage: partition_problem [-lists <int>] [-length <int>] [-p <int>] \
       [-verbose] [-d <int>]");
  (!n, !l, !p, !v, !d)

module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type t =
    | Unpartitioned
    | Partitioned
    | List of int list
    | Result of (int list * int list) option
    | Done of int
  [@@deriving equal, hash]

  let to_string =
    let string_of_int_list l =
      List.map string_of_int l |> String.concat "; " |> Format.sprintf "[%s]"
    in
    function
    | Unpartitioned -> "Unpartitioned"
    | Partitioned -> "Partitioned"
    | List l -> string_of_int_list l
    | Result (Some (l1, l2)) ->
        Format.sprintf "(%s, %s)" (string_of_int_list l1)
          (string_of_int_list l2)
    | Result None -> "No partition found"
    | Done i -> Format.sprintf "Done %d" i
end

open Linda.Make (Element)

let string_of_tuple t = List.map Element.to_string t |> String.concat " ; "

let generate n =
  let nums = List.init n (fun _ -> Random.int 100 + 1) in
  if List.fold_left ( + ) 0 nums mod 2 = 0 then nums else 1 :: nums

let fill ts () =
  for _ = 1 to num_lists do
    out ts [ Unpartitioned; List (generate list_len) ]
  done;
  if verbose then
    Format.printf "Inserted %d lists, current space: %s\n%!" num_lists
      (string_of_tuplespace ts string_of_tuple);
  []

let find_partition nums =
  let sum = List.fold_left ( + ) 0 nums in
  if sum mod 2 <> 0 then None
  else
    let target = sum / 2 in
    let dp = Array.make (target + 1) None in
    (* A sum of 0 is always possible with an empty subset *)
    dp.(0) <- Some [];

    let update_dp num =
      for i = target downto num do
        match dp.(i - num) with
        | Some subset -> dp.(i) <- Some (num :: subset)
        | None -> ()
      done
    in

    List.iter update_dp nums;

    let rec ( - ) l1 = function
      | [] -> l1
      | x :: xs ->
          let rec rm_first = function
            | [] -> []
            | y :: ys -> if x = y then ys else y :: rm_first ys
          in
          rm_first l1 - xs
    in

    let ( >>| ) x f = Option.map f x in

    dp.(target) >>| List.rev >>| fun subset1 -> (subset1, nums - subset1)

(* Test the performance of each function *)
let _test () =
  Random.self_init ();

  let nums = generate 500 in

  Format.printf "Generated list: [%s] sum: %d\n"
    ( List.to_seq nums |> Seq.take 5 |> Seq.map string_of_int
    |> Seq.fold_left (fun acc s -> acc ^ s ^ "; ") " "
    |> fun s -> s ^ "... " )
    (List.fold_left ( + ) 0 nums);

  let p = ref None in

  let rec run_find_partition n =
    if n = 0 then []
    else
      let t = Sys.time () in
      p := find_partition nums;
      let elapsed_time = Sys.time () -. t in
      elapsed_time :: run_find_partition (n - 1)
  in

  let num_runs = 10 in
  let partition_times = run_find_partition num_runs in

  let avg_time =
    List.fold_left ( +. ) 0. partition_times /. float_of_int num_runs
  in

  Format.printf "Average elapsed time for find_partition: %f\n" avg_time;

  let () =
    print_endline "find_partition result:";
    match !p with
    | Some (subset1, subset2) ->
        Format.printf "Subset 1: [%s] sum: %d\n"
          ( List.to_seq subset1 |> Seq.take 5 |> Seq.map string_of_int
          |> Seq.fold_left (fun acc s -> acc ^ s ^ "; ") " "
          |> fun s -> s ^ "... " )
          (List.fold_left ( + ) 0 subset1);
        Format.printf "Subset 2: [%s] sum: %d\n"
          ( List.to_seq subset2 |> Seq.take 5 |> Seq.map string_of_int
          |> Seq.fold_left (fun acc s -> acc ^ s ^ "; ") " "
          |> fun s -> s ^ "... " )
          (List.fold_left ( + ) 0 subset2)
    | None -> print_endline "No partition found"
  in

  match !p with
  | Some (subset1, subset2) ->
      let s1 = List.fold_left ( + ) 0 subset1 in
      let s2 = List.fold_left ( + ) 0 subset2 in
      if s1 <> s2 then failwith "Subsets do not have equal sum"
  | None -> ()

let partition_problem ts =
  (* let generator = eval ts (fill ts) in *)
  let _ = fill ts () in

  (* let t = Sys.time () in *)
  let partitioners =
    List.init num_partitioners (fun i ->
        eval ts (fun () ->
            let rec loop () =
              match inp ts [ Match Unpartitioned; Wildcard ] with
              | Some [ Unpartitioned; List l ] ->
                  out ts [ Partitioned; List l; Result (find_partition l) ];
                  loop ()
              | None -> [ Element.Done i ]
              | _ -> assert false
            in
            loop ()))
  in

  (* List.iter (await ts) (generator :: sorters); *)
  List.iter (await ts) partitioners;
  (* Format.printf "Elapsed time: %f\n%!" (Sys.time () -. t); *)
  assert (inp ts [ Match Unpartitioned; Wildcard ] = None);
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
  run ts (fun () -> partition_problem ts);
  (* Destroy the tuplespace *)
  destroy_tuplespace ts
