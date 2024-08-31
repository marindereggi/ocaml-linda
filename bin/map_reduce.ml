module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type int_list = int list [@@deriving equal, hash]

  let string_of_int_list l =
    List.map string_of_int l |> String.concat "; " |> Format.sprintf "[%s]"

  type esc_string = string [@@deriving equal, hash]

  let string_of_esc_string s = String.escaped s |> Format.sprintf "\"%s\""

  (* Tuplespace with heterogeneous tuples *)
  type t =
    | Map
    | Reduce
    | Result
    | Done
    | Int of int [@nested ""]
    | List of int_list [@nested ""]
    | String of esc_string [@nested ""]
  [@@deriving equal, hash, to_string]
end

open Linda.Make (Element)

let string_of_tuple t = List.map Element.to_string t |> String.concat " "
let print_tuple t = print_endline @@ "Read tuple: " ^ string_of_tuple t

let demo_run ts =
  let p =
    eval ts (fun () ->
        out ts [ Map; List [ 1; 2; 3 ] ];
        out ts [ Map; List [ 4; 5; 6 ] ];
        out ts [ Map; List [ 7; 8; 9 ] ];
        out ts [ Map; List [ 1; 3; 5; 7; 9; 11; 13; 15; 17; 19 ] ];

        Format.printf "Inserted tuples, current space: { %s }\n%!"
          (string_of_tuplespace ts string_of_tuple);
        [ Done; String "inserted" ])
  in

  print_endline "Reading tuples...";

  let map_worker =
    eval ts (fun () ->
        let rec mapfn () =
          let t = in_ ts [ Match Map; Wildcard ] in
          print_tuple t;
          let l =
            match t with
            | [ Map; List l ] -> l
            | _ -> failwith "Invalid map tuple"
          in
          let l' = List.map (( + ) 1) l in
          out ts [ Reduce; List l' ];
          mapfn ()
        in
        mapfn ())
  in

  let reduce_worker =
    eval ts (fun () ->
        let rec reducefn () =
          let t = in_ ts [ Match Reduce; Wildcard ] in
          print_tuple t;
          let l =
            match t with
            | [ Reduce; List l ] -> l
            | _ -> failwith "Invalid reduce tuple"
          in
          let sum = List.fold_left ( + ) 0 l in
          out ts [ Result; Int sum ];
          reducefn ()
        in
        reducefn ())
  in

  print_endline "Done";

  Format.printf "After reading, current space: { %s }\n%!"
    (string_of_tuplespace ts string_of_tuple);

  await ts p;
  await ts map_worker;
  await ts reduce_worker

let () =
  (* Create a new tuplespace *)
  let ts = create_tuplespace () in

  Domain.spawn (fun () ->
      let rec loop i =
        Format.printf "\027[32mCurrent space: { %s }\027[0m\n%!"
          (string_of_tuplespace ts string_of_tuple);
        if i < 2 then (
          Unix.sleep 1;
          loop (i + 1))
        else exit 0
      in
      loop 0)
  |> ignore;

  (* Run the tuple space *)
  run ts (fun () -> demo_run ts);

  (* Destroy the tuplespace *)
  destroy_tuplespace ts
