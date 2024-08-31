(* Define the number of philosophers *)
let num_philosophers, time =
  let p = ref 5 and t = ref 1 in
  Arg.(
    parse
      [
        ("-philosophers", Set_int p, "number of philosophers");
        ("-time", Set_int t, "time in seconds");
      ]
      ignore "Usage: dining_philosophers [-philosophers <int>]");
  (!p, !t)

module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type t = Fork of int [@@deriving equal, hash]
end

open Linda.Make (Element)

(* Function to simulate a philosopher's actions *)
let philosopher ts id =
  let left_fork = id and right_fork = (id + 1) mod num_philosophers in

  let rec loop () =
    (* STAGE 1: Thinking *)
    Format.printf "[%d] is thinking\n%!" id;
    Unix.sleep (Random.int time + 1);

    (* STAGE 2: Hungry *)
    Format.printf "[%d] is hungry\n%!" id;

    (* Try to pick up left fork *)
    match inp ts [ Match (Fork left_fork) ] with
    | None ->
        (* Left fork not available, continue thinking *)
        loop ()
    | Some left -> (
        match inp ts [ Match (Fork right_fork) ] with
        | None ->
            (* If right fork is not available, put left fork back *)
            out ts left;

            (* Continue thinking *)
            loop ()
        | Some right ->
            (* Both forks are available, start eating *)

            (* STAGE 3: Eating *)
            Format.printf "[%d] is eating (got forks %d and %d)\n%!" id
              left_fork right_fork;
            Unix.sleep (Random.int time + 1);

            (* Release forks *)
            out ts left;
            out ts right;

            (* Finished *)
            [])
  in
  loop ()

(* Function to start the simulation *)
let dining_philosophers ts =
  (* Initialize forks *)
  for i = 0 to num_philosophers - 1 do
    out ts [ Fork i ]
  done;

  (* Spawn philosophers *)
  let philosophers =
    List.init num_philosophers (fun id -> eval ts (fun () -> philosopher ts id))
  in

  (* Wait for all philosophers to finish *)
  List.iter (await ts) philosophers

(* Run the simulation *)
let () =
  Random.self_init ();
  (* Create a new tuplespace *)
  let ts = create_tuplespace () in
  (* Run the dining philosophers simulation *)
  run ts (fun () -> dining_philosophers ts);
  (* Clean up *)
  destroy_tuplespace ts
