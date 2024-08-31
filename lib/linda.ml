module OrderedSet = struct
  (* module IntSet = Set.Make (Int) *)
  type 'a t = { mutable lst : 'a list; lock : Mutex.t }

  let create () = { lst = []; lock = Mutex.create () }

  let get { lst; lock } eq =
    (* we need read locks *)
    Mutex.protect lock (fun () -> List.find_opt eq lst)

  (* let remove set eq =
     Mutex.protect set.lock (fun () ->
         let rec remove = function
           | [] -> ([], `Error)
           | x :: xs ->
               if eq x then (xs, `Ok)
               else
                 let xs', r = remove xs in
                 (x :: xs', r)
         in
         let lst', r = remove set.lst in
         set.lst <- lst';
         r) *)

  let add set compare x =
    Mutex.protect set.lock (fun () ->
        let rec add = function
          | [] -> ([ x ], `Ok)
          | y :: ys as l -> (
              match compare x y with
              | c when c < 0 -> (x :: l, `Ok)
              | 0 -> (l, `Error)
              | _ ->
                  let l', r = add ys in
                  (y :: l', r))
        in
        let lst', r = add set.lst in
        set.lst <- lst';
        r)

  let map f set = Mutex.protect set.lock (fun () -> List.map f set.lst)
end

module LockList = struct
  type 'a t = { mutable lst : 'a list; lock : Mutex.t }

  let create () = { lst = []; lock = Mutex.create () }
  let add list x = Mutex.protect list.lock (fun () -> list.lst <- x :: list.lst)

  let remove list eq =
    Mutex.protect list.lock (fun () ->
        let rec remove = function
          | [] -> ([], None)
          | x :: xs ->
              if eq x then (xs, Some x)
              else
                let xs', r = remove xs in
                (x :: xs', r)
        in
        let lst', r = remove list.lst in
        list.lst <- lst';
        r)

  let get list eq =
    Mutex.protect list.lock (fun () -> List.find_opt eq list.lst)

  let map f list = Mutex.protect list.lock (fun () -> List.map f list.lst)
end

module Make (T : Hashtbl.HashedType) = struct
  (* Define the type for tuples *)
  type tuple = T.t list

  module Node = struct
    type t = Node of (int * t) OrderedSet.t | Leaf of tuple LockList.t

    let create () = Node (OrderedSet.create ())

    (**
      * Add a tuple to the tuplespace
      * @param root The root node of the tuplespace
      * @param tuple The tuple to add
      * @param d The modulo value for hashing
      *
      * The algorithm works as follows:
      * 1. Compute the hashes of the tuple, so for example ["a"; "b"; "c"] would be [hash "a" mod d; hash "b" mod d; hash "c" mod d] -> [5; 2; 3]
      * 2. Now we traverse the tree along the hashes, so for each depth we check if the node with the calculated hash exists
      * 3. If the node exists, we continue with the next hash, otherwise we create a new node and continue with the next hash
      * 4. If we reach the end of the hashes, we check the leaf node with "hash" value -1 add the tuple to the leaf node,
      * if it does not exist, we create a new leaf node and add the tuple
      * For example, if we have the tuple ["a"; "b"; "c"] and the hashes [5; 2; 3], the tree would look like this:
      * Node (5, Node (2, Node (3, Node (-1, Leaf [ ["a"; "b"; "c"] ]))))
    *)
    let add root tuple d =
      let hashes = List.map (fun x -> T.hash x mod d) tuple in
      let rec add_tuple node t hashes =
        match node with
        | Leaf _ -> failwith "Invalid node: Leaf"
        | Node n -> (
            match hashes with
            | [] -> (
                match OrderedSet.get n (fun (h, _) -> h = -1) with
                | Some (-1, Leaf l) -> LockList.add l tuple
                | None ->
                    let leaf = LockList.create () in
                    LockList.add leaf tuple;
                    ignore
                    @@ OrderedSet.add n
                         (fun (h, _) (h', _) -> Int.compare h h')
                         (-1, Leaf leaf)
                | _ -> failwith "Invalid tuple")
            | h :: hs -> (
                match OrderedSet.get n (fun (h', _) -> h' = h) with
                | Some (_, n') -> add_tuple n' t hs
                | _ ->
                    let node = OrderedSet.create () in
                    ignore
                    @@ OrderedSet.add n
                         (fun (h', _) (h'', _) -> Int.compare h' h'')
                         (h, Node node);
                    add_tuple (Node node) t hs))
      in
      add_tuple root tuple hashes

    let get root (pattern : T.t option list) d =
      let hashes = List.map (Option.map (fun x -> T.hash x mod d)) pattern in
      let rec get_leaves node hashes =
        match node with
        | Leaf _ -> failwith "Invalid node: Leaf"
        | Node n -> (
            match hashes with
            | [] -> (
                match OrderedSet.get n (fun (h, _) -> h = -1) with
                | Some (-1, Leaf l) -> [ l ]
                | _ -> [])
            | Some h :: hs -> (
                match OrderedSet.get n (fun (h', _) -> h' = h) with
                | Some (_, n') -> get_leaves n' hs
                | _ -> [])
            | None :: hs ->
                n
                |> OrderedSet.map (function
                     | h, (Node _ as n') when h != -1 -> get_leaves n' hs
                     | _ -> [])
                |> List.concat)
      in
      get_leaves root hashes
      |> List.find_map (fun l ->
             LockList.get l
               (List.for_all2
                  (fun p x ->
                    match p with None -> true | Some y -> T.equal x y)
                  pattern))

    let remove root (pattern : T.t option list) d =
      let hashes = List.map (Option.map (fun x -> T.hash x mod d)) pattern in
      let rec get_leaves node hashes =
        match node with
        | Leaf _ -> failwith "Invalid node: Leaf"
        | Node n -> (
            match hashes with
            | [] -> (
                match OrderedSet.get n (fun (h, _) -> h = -1) with
                | Some (-1, Leaf l) -> [ l ]
                | _ -> [])
            | Some h :: hs -> (
                match OrderedSet.get n (fun (h', _) -> h' = h) with
                | Some (_, n') -> get_leaves n' hs
                | _ -> [])
            | None :: hs ->
                n
                |> OrderedSet.map (function
                     | h, (Node _ as n') when h != -1 -> get_leaves n' hs
                     | _ -> [])
                |> List.concat)
      in
      get_leaves root hashes
      |> List.find_map (fun l ->
             LockList.remove l
               (List.for_all2
                  (fun p x ->
                    match p with None -> true | Some y -> T.equal x y)
                  pattern))

    (** Function [map_t] maps a function [f] over all tuples in the tuplespace *)
    let rec map_t f = function
      | Leaf l -> LockList.map f l
      | Node n -> n |> OrderedSet.map (fun (_, n') -> map_t f n') |> List.concat
  end

  open Domainslib

  type tuplespace = {
    space : Node.t;
    (* condition : Condition.t; *)
    pool : Task.pool;
    d : int;
  }

  type pattern = Match of T.t | Wildcard

  type promise = unit Task.promise
  (** Define the type for promises *)

  (* Create a new tuplespace *)
  let create_tuplespace ?(num_domains = Domain.recommended_domain_count () - 1)
      ?(d = 10) () =
    {
      space = Node.create ();
      (* condition = Condition.create (); *)
      pool = Task.setup_pool ~num_domains ();
      d;
    }

  (* Destroy a tuplespace *)
  let destroy_tuplespace { pool; _ } = Task.teardown_pool pool

  (* Run the tuple space *)
  let run { pool; _ } = Task.run pool

  (* Add a tuple to the tuplespace *)
  let out { space; d; _ } tuple = Node.add space tuple d

  let transform pattern =
    List.map (function Match v -> Some v | Wildcard -> None) pattern

  (* Read a tuple from the tuplespace *)
  let rd { space; d; _ } pattern =
    let rec wait_for_tuple () =
      match Node.get space (transform pattern) d with
      | Some tuple -> tuple
      | None -> wait_for_tuple ()
    in
    wait_for_tuple ()

  (* Take a tuple from the tuplespace *)
  let in_ { space; d; _ } pattern =
    let rec wait_for_tuple () =
      match Node.remove space (transform pattern) d with
      | Some tuple -> tuple
      | None -> wait_for_tuple ()
    in
    wait_for_tuple ()

  (* Read a tuple from the tuplespace without blocking *)
  let rdp { space; d; _ } pattern = Node.get space (transform pattern) d

  (* Take a tuple from the tuplespace without blocking *)
  let inp { space; d; _ } pattern = Node.remove space (transform pattern) d
  let inp_notry = inp

  (* Evaluate a tuple, then put it in the tuplespace *)
  let eval ts tuple = Task.async ts.pool (fun () -> out ts (tuple ()))

  (* Perform a computation concurrently *)
  let work { pool; _ } = Task.async pool

  (* Wait for a promise to be fulfilled *)
  let await { pool; _ } = Task.await pool

  let string_of_tuplespace { space; _ } string_of_tuple =
    space |> Node.map_t string_of_tuple |> String.concat ", "
end
