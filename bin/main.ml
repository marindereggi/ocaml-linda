module Element = struct
  open Ppx_compare_lib.Builtin
  open Ppx_hash_lib.Std.Hash.Builtin

  type t = Int of int | Float of float | Char of char | String of string
  [@@deriving equal, hash]

  let to_string = function
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | String s -> "\"" ^ String.escaped s ^ "\""
    | Char c -> "'" ^ Char.escaped c ^ "'"
end

open Linda.Make (Element)

let string_of_tuple t =
  List.map Element.to_string t |> String.concat "; " |> Format.sprintf "[%s]"

let print_tuple t = print_endline @@ "Read tuple: " ^ string_of_tuple t

let print_tuple_op = function
  | Some t -> print_tuple t
  | None -> print_endline "No match found"

let demo_run ts =
  let p =
    eval ts (fun () ->
        out ts [ Int 1; String "a" ];
        out ts [ Int 2; String "b" ];
        out ts [ Int 2; String "d" ];
        out ts [ Int 3; String "c"; Int 4 ];
        out ts [ Char 'c' ];
        let open Float in
        let p1 = eval ts (fun () -> [ Int 5; Float (pow 2. 3.) ])
        and p2 = eval ts (fun () -> [ Int 6; Float (sqrt 9.) ]) in
        await ts p1;
        await ts p2;

        Format.printf "Inserted tuples, current space: %s\n%!"
          (string_of_tuplespace ts string_of_tuple);
        [ String "Done" ])
  in

  print_endline "Reading tuples...";

  (* Non-blocking read *)
  let t = rdp ts [ Match (Int 2); Wildcard ] in
  print_tuple_op t;

  (* Blocking read *)
  let t = rd ts [ Match (Int 1); Wildcard ] in
  print_tuple t;

  (* Blocking take *)
  let t = in_ ts [ Wildcard; Match (Float 3.) ] in
  print_tuple t;

  (* Non-blocking take *)
  let t = inp ts [ Match (Int 2); Wildcard ] in
  print_tuple_op t;

  (* Non-blocking take *)
  let t = inp ts [ Match (Int 2); Wildcard ] in
  print_tuple_op t;

  print_endline "Done";

  Format.printf "After reading, current space: %s\n%!"
    (string_of_tuplespace ts string_of_tuple);

  await ts p

let () =
  (* Create a new tuplespace *)
  let ts = create_tuplespace () in

  (* Run the tuple space *)
  run ts (fun () -> demo_run ts);

  (* Destroy the tuplespace *)
  destroy_tuplespace ts
