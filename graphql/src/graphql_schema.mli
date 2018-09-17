(** GraphQL schema functor *)

(* IO signature *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Stream *)
module type Stream = sig
  type +'a io
  type 'a t

  val map : 'a t -> ('a -> 'b io) -> 'b t
end

(* Err *)
module type Err =
  sig
    type t
    val message_of_error : t -> string
    val error_of_message : string -> t
  end


(* GraphQL schema functor *)
module Make (Io : IO) (Stream : Stream with type 'a io = 'a Io.t) (Err : Err) :
  Graphql_intf.Schema with type 'a io = 'a Io.t
                      and type 'a stream = 'a Stream.t
                      and type err = Err.t
