type ingredient = Tobacco | Paper | Matches
[@@deriving equal, hash, to_string]

let ingredients = [ Tobacco; Paper; Matches ]

module Element = struct
  type t = Agent | Ingredient of ingredient [@@deriving equal, hash]
end

open Linda.Make (Element)

let smoker ts id =
  (* Get the values of the other two ingredients *)
  let i1, i2 =
    match List.filteri (fun i _ -> i <> id) ingredients with
    | [ i1; i2 ] -> (i1, i2)
    | _ -> assert false
  in

  let rec smoke () =
    (* Pick up the first missing ingredient *)
    let first = in_ ts [ Match (Ingredient i1) ] in

    (* Try to pick up the second missing ingredient *)
    let () =
      match inp ts [ Match (Ingredient i2) ] with
      | Some _ ->
          (* Start smoking *)
          Format.printf "Smoker %d is smoking (got %s and %s)\n%!" id
            (string_of_ingredient i1) (string_of_ingredient i2);

          (* Simulate smoking *)
          Unix.sleep 1;

          (* Signal that smoking is done *)
          out ts [ Agent ]
      | None ->
          (* Couldn't get the second ingredient, put the first one back *)
          out ts first
    in

    (* Continue smoking *)
    smoke ()
  in
  smoke ()

let agent ts =
  let rec provide_ingredients () =
    (* Wait for the signal to provide ingredients *)
    let _ = in_ ts [ Match Agent ] in

    (* Choose a random ingredient to not put on the table *)
    let i = Random.int 3 in

    (* Remove the chosen ingredient from the list of all ingredients *)
    match List.filteri (fun j _ -> j <> i) ingredients with
    | [ i1; i2 ] ->
        (* Put the remaining ingredients on the table *)
        out ts [ Ingredient i1 ];
        out ts [ Ingredient i2 ];

        Format.printf "\nAgent put %s and %s on the table\n%!"
          (string_of_ingredient i1) (string_of_ingredient i2);

        (* Continue providing ingredients *)
        provide_ingredients ()
    | _ -> assert false
  in
  provide_ingredients ()

let run_simulation ts =
  (* Initialize the agent semaphore *)
  out ts [ Agent ];

  (* Start the agent *)
  let agent = eval ts (fun () -> agent ts) in

  (* Start the smokers *)
  let smokers = List.init 3 (fun i -> eval ts (fun () -> smoker ts i)) in

  (* Wait for all smokers to finish (which they never will in this case) *)
  List.iter (await ts) (agent :: smokers)

let () =
  (* Initialize the random number generator *)
  Random.self_init ();
  (* Create a tuple space *)
  let ts = create_tuplespace () in
  (* Run the simulation *)
  run ts (fun () -> run_simulation ts);
  (* Destroy the tuple space *)
  destroy_tuplespace ts
