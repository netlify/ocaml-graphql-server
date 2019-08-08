(* Err *)
module type Err = sig
  type t

  val message_of_error : t -> string
  val extensions_of_error : t -> (string * Yojson.Basic.json) list
end

module type Context = sig
  type t
end

(* GraphQL schema functor *)
module Make (Io : Graphql_intf.IO) (Err : Err) (Context : Context):
  Graphql_intf.Schema with module Io = Io
     and type err = Err.t
     and type ctx = Context.t
