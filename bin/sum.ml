module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type t = Num | Sum | Done | Val of int [@nested ""]
  [@@deriving equal, hash, to_string]
end

open Linda.Make (Element)

let string_of_tuple t =
  List.map Element.to_string t |> String.concat " " |> Format.sprintf "(%s)"

let demo_run ts =
  let p =
    eval ts (fun () ->
        (* Seq.(ints 1 |> take 10 |> iter (fun i -> out ts [ Num i ])); *)
        for i = 1 to 100 do
          out ts [ Num; Val i ]
        done;

        Format.printf "Inserted tuples, current space: %s\n%!"
          (string_of_tuplespace ts string_of_tuple);
        [ Sum; Val 0 ])
  in

  await ts p;

  print_endline "Summing numbers...";

  List.init 3 (fun x ->
      let x = x + 1 in
      Format.printf "Starting sum [%d]\n%!" x;

      eval ts (fun () ->
          let rec loop () =
            match inp ts [ Match Num; Wildcard ] with
            | Some [ Num; Val i ] -> (
                match in_ ts [ Match Sum; Wildcard ] with
                | [ Sum; Val s ] ->
                    Format.printf "[%d] Got: (Num %d) + (Sum %d) = Sum %d\n%!" x
                      i s (s + i);
                    out ts [ Sum; Val (s + i) ];
                    (* Unix.sleep 1; *)
                    loop ()
                | exception e ->
                    failwith ("Exception in inp: " ^ Printexc.to_string e)
                | _ -> failwith "No sum found")
            | None ->
                Format.printf "[%d] is done\n%!" x;
                Element.[ Done; Val x ]
            | _ -> failwith "Invalid tuple"
          in
          loop ()))
  |> List.iter (await ts);

  print_endline "Done";

  Format.printf "After reading, current space: %s\n%!"
    (string_of_tuplespace ts string_of_tuple);

  match inp ts [ Match Sum; Wildcard ] with
  | Some [ Sum; Val s ] -> Format.printf "Final sum: %d\n%!" s
  | _ -> failwith "No sum found"

let () =
  (* Create a new tuplespace *)
  let ts = create_tuplespace () in

  (* Run the tuple space *)
  run ts (fun () -> demo_run ts);

  (* Destroy the tuplespace *)
  destroy_tuplespace ts
