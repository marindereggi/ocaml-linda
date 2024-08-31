let num_customers, num_chairs =
  let n = ref 10 and c = ref 3 in
  Arg.(
    parse
      [
        ("-customers", Set_int n, "number of customers");
        ("-chairs", Set_int c, "number of chairs in waiting room");
      ]
      ignore "Usage: sleeping_barber [-customers <int>] [-chairs <int>]");
  (!n, !c)

module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type t =
    | Barber
    | Customer
    | BarberWakeUp
    | CustomersWaiting
    | Done
    | Left
    | Int of int [@nested ""]
  [@@deriving equal, hash, to_string]
end

open Linda.Make (Element)

let string_of_tuple t = List.map Element.to_string t |> String.concat " "

let barber ts =
  let rec get_customer i =
    (* Prevent starvation by processing customers in order by their id *)
    match inp ts [ Match Customer; Match (Int i) ] with
    | Some [ Customer; Int id ] -> id
    | _ -> get_customer (i + 1)
  in

  let cut_hair i =
    (* Check if there are customers waiting *)
    match in_ ts [ Match CustomersWaiting; Wildcard ] with
    | [ CustomersWaiting; Int n ] when n > 0 ->
        (* Get customer in waiting room with the lowest id *)
        let id = get_customer i in

        (* Make seat available for next customer *)
        out ts [ CustomersWaiting; Int (n - 1) ];

        (* Cut hair *)
        Format.printf "Barber is cutting hair of customer %d\n%!" id;
        Unix.sleep (Random.int 5 + 1);

        (* Notify customer that haircut is done *)
        out ts [ Customer; Int id; Done ];
        id + 1
    | _ -> i
  in

  let rec barber_sleep i =
    (* Wait for a customer to wake up the barber *)
    let _ = in_ ts [ Match BarberWakeUp ] in
    (* Return if all customers have been served *)
    match inp ts [ Match Barber; Match Done ] with
    | Some d -> d
    | None -> cut_hair i |> barber_sleep
  in
  barber_sleep 0

let customer ts id =
  (* Check if there is a chair available *)
  match in_ ts [ Match CustomersWaiting; Wildcard ] with
  | [ CustomersWaiting; Int n ] when n < num_chairs ->
      (* Enter waiting room *)
      out ts [ Customer; Int id ];
      (* Take a seat *)
      out ts [ CustomersWaiting; Int (n + 1) ];
      (* Wake up barber if sleeping *)
      out ts [ BarberWakeUp ];

      Format.printf "Customer %d entered waiting room\n%!" id;

      (* Wait for barber to finish *)
      let finished = in_ ts [ Match Customer; Match (Int id); Match Done ] in
      Format.printf "Customer %d is done\n%!" id;
      finished
  | full ->
      (* Waiting room is full *)
      out ts full;
      Format.printf "Customer %d left (waiting room full)\n%!" id;
      [ Customer; Int id; Left ]

(* Sleeping barber simulation *)
let sleeping_barber ts =
  (* Initialize waiting room *)
  out ts [ CustomersWaiting; Int 0 ];

  (* Start barber process *)
  let barber = eval ts (fun () -> barber ts) in

  print_endline "Barber is ready to cut hair";

  (* Generate customers *)
  let customers =
    let rec loop = function
      | n when n >= num_customers -> []
      | n ->
          let customer = eval ts (fun () -> customer ts n) in
          (* Random delay between customers *)
          Unix.sleep (Random.int 3 + 1);
          customer :: loop (n + 1)
    in
    loop 0
  in

  (* Wait for all customers to be served or leave *)
  List.iter (await ts) customers;

  print_endline "All customers have been served";

  (* Notify barber that all customers have been served *)
  out ts [ Barber; Done ];
  out ts [ BarberWakeUp ];

  (* Wait for barber to finish *)
  await ts barber;

  Format.printf "Barber is done cutting hair.\nFinal state: %s\n"
    (string_of_tuplespace ts string_of_tuple)

(* Run the simulation *)
let () =
  (* Initialize the random number generator *)
  Random.self_init ();
  (* Create a new tuplespace *)
  let ts = create_tuplespace () in
  (* Run the sleeping barber simulation *)
  run ts (fun () -> sleeping_barber ts);
  (* Clean up *)
  destroy_tuplespace ts
