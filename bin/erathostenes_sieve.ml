let max_num, num_workers, verbose, silent, d, n_intervals =
  let n = ref 100
  and w = ref 1
  and v = ref false
  and s = ref false
  and d = ref 10
  and i = ref 10 in
  Arg.(
    parse
      [
        ("-max", Set_int n, "maximum number to check");
        ("-workers", Set_int w, "number of worker processes");
        ("-verbose", Set v, "print tuplespace after insert");
        ("-silent", Set s, "do not print tuplespace");
        ("-d", Set_int d, "hash parameter");
        ("-intervals", Set_int i, "number of intervals of the table");
      ]
      ignore
      "Usage: erathostenes_sieve [-max <int>] [-workers <int>] [-verbose] \
       [-silent] [-d <int>] [-divisions <int>]");
  (!n, !w, !v, !s, !d, !i)

module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type int_list = int list [@@deriving equal, hash]

  let string_of_int_list lst =
    List.map string_of_int lst |> String.concat "; " |> Format.sprintf "[%s]"

  type prime_array = bool array [@@deriving equal]

  let hash_fold_prime_array = hash_fold_array_frozen hash_fold_bool

  let string_of_prime_array a =
    Array.to_seq a |> Seq.map string_of_bool |> List.of_seq
    |> String.concat "; " |> Format.sprintf "[%s]"

  let int_list_of_prime_array start arr =
    Array.to_seq arr
    |> Seq.mapi (fun i x -> if x then Some (start + i) else None)
    |> Seq.filter_map (fun x -> x)
    |> List.of_seq

  type t =
    | Numbers
    | Smallest_prime
    | Array of prime_array [@nested ""]
    | Prime of int [@nested ""]
    | Start of int [@nested "s:"]
    | Iteration of int [@nested "i:"]
  [@@deriving equal, hash, to_string]

  let string_of_tuple t =
    let to_string_list = function
      | [ Numbers; Iteration i; Start s; Array arr ] ->
          let primes = arr |> int_list_of_prime_array s |> string_of_int_list in
          List.map to_string [ Numbers; Iteration i; Start s ] @ [ primes ]
      | t -> List.map to_string t
    in
    t |> to_string_list |> String.concat " "
end

open Linda.Make (Element)

let fill ts =
  let base_len = (max_num - 1) / n_intervals in
  let rem = (max_num - 1) mod n_intervals in

  for d = 0 to n_intervals - 1 do
    let start = 2 + (d * base_len) + min d rem
    and len = base_len + Bool.to_int (d < rem) in
    let arr = Array.make len true in
    out ts [ Numbers; Iteration 0; Start start; Array arr ]
  done;

  out ts [ Smallest_prime; Prime 2 ];

  if verbose then
    Format.printf "Inserted numbers from 2 to %d\nCurrent space: %s\n%!" max_num
      (string_of_tuplespace ts Element.string_of_tuple)

(** Filter out multiples of a given number from a list *)
let filter_multiples p start arr =
  let len = Array.length arr in
  let rec loop n =
    if n < len then (
      arr.(n) <- false;
      loop (n + p))
  in
  let n = (p * p) - start in
  if n >= 0 then loop n
  else
    let k = ((-n - 1) / p) + 1 in
    loop (n + (k * p))

(** Update the smallest prime in the tuplespace *)
let update_smallest ts prime start arr =
  let update p =
    match in_ ts [ Match Smallest_prime; Wildcard ] with
    | [ Smallest_prime; Prime p' ] ->
        out ts [ Smallest_prime; Prime (min p p') ]
    | _ -> assert false
  in
  let len = Array.length arr in
  let rec loop = function
    | n when n < len -> if arr.(n) then Some (n + start) else loop (n + 1)
    | _ -> None
  in
  loop (max 0 (prime - start + 1)) |> Option.iter update

(** Handle prime filtering for a specific worker *)
let handle_primes ts i prime =
  let rec loop () =
    match inp ts [ Match Numbers; Match (Iteration i); Wildcard; Wildcard ] with
    | Some [ Numbers; Iteration i; Start s; Array arr ] ->
        filter_multiples prime s arr;
        update_smallest ts prime s arr;
        out ts [ Numbers; Iteration (i + 1); Start s; Array arr ];
        loop ()
    | _ -> ()
  in
  loop ()

(** Main sieve loop and coordination *)
let sieve ts =
  let sqrt_n = max_num |> float_of_int |> sqrt |> ceil |> int_of_float in
  let rec loop i =
    let prime =
      match in_ ts [ Match Smallest_prime; Wildcard ] with
      | [ Smallest_prime; Prime i ] -> i
      | _ -> assert false
    in
    if prime <= sqrt_n then (
      out ts [ Smallest_prime; Prime (max_num + 1) ];
      (fun _ -> work ts (fun () -> handle_primes ts i prime))
      |> List.init num_workers
      |> List.iter (await ts);
      loop (i + 1))
  in
  loop 0

(** Main function to run the Sieve of Eratosthenes *)
let sieve_of_eratosthenes ts =
  (* Fill the tuplespace with the numbers from 2 to max_num *)
  fill ts;

  (* Sieve the numbers *)
  sieve ts;

  if verbose then
    Format.printf "Final space (primes): %s\n%!"
      (string_of_tuplespace ts Element.string_of_tuple);

  (* Extract the primes from the tuplespace *)
  if not silent then
    (fun _ ->
      match in_ ts [ Match Numbers; Wildcard; Wildcard; Wildcard ] with
      | [ Numbers; Iteration _; Start start; Array arr ] ->
          Element.int_list_of_prime_array start arr
      | _ -> assert false)
    |> Seq.init n_intervals
    |> Seq.fold_left (List.merge Int.compare) []
    |> Element.string_of_int_list
    |> Format.printf "Primes: %s\n%!"

(* Entry point *)
let () =
  (* Create a tuplespace *)
  let ts = create_tuplespace ~d () in
  (* Run the sieve *)
  run ts (fun () -> sieve_of_eratosthenes ts);
  (* Destroy the tuplespace *)
  destroy_tuplespace ts
