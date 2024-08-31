module Make (T : Hashtbl.HashedType) : sig
  type tuple = T.t list
  (** Type for tuples *)

  type tuplespace
  (** Type for the tuplespace *)

  type promise
  (** Type for promises *)

  type pattern = Match of T.t | Wildcard  (** Type for patterns *)

  val create_tuplespace : ?num_domains:int -> ?d:int -> unit -> tuplespace
  (** Create a new tuplespace *)

  val destroy_tuplespace : tuplespace -> unit
  (** Destroy a tuplespace *)

  val run : tuplespace -> (unit -> 'a) -> 'a
  (** Run the tuple space *)

  val out : tuplespace -> tuple -> unit
  (** Add a tuple to the tuplespace *)

  val rd : tuplespace -> pattern list -> tuple
  (** Read a tuple from the tuplespace *)

  val in_ : tuplespace -> pattern list -> tuple
  (** Take a tuple from the tuplespace *)

  val rdp : tuplespace -> pattern list -> tuple option
  (** Read a tuple from the tuplespace without blocking *)

  val inp : tuplespace -> pattern list -> tuple option
  (** Take a tuple from the tuplespace without blocking *)

  val inp_notry : tuplespace -> pattern list -> tuple option
  (** Take a tuple from the tuplespace without blocking *)

  val eval : tuplespace -> (unit -> tuple) -> promise
  (** Evaluate a tuple concurrently, then put it in the tuplespace *)

  val work : tuplespace -> (unit -> unit) -> promise
  (** Run a computation concurrently *)

  val await : tuplespace -> promise -> unit
  (** Wait for a promise to be fulfilled *)

  val string_of_tuplespace : tuplespace -> (tuple -> string) -> string
  (** Convert a tuplespace to a string *)
end
