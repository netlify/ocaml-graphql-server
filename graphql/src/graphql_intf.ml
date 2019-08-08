module StringMap = struct
  include Map.Make(String)
  exception Missing_key of string
  let find_exn key t = try find key t with Not_found -> raise (Missing_key key)
  let find k t = try Some (find_exn k t) with Missing_key _ -> None
end

(* IO signature *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Stream : sig
    type 'a t
    type +'a io

    val map : 'a t -> ('a -> 'b io) -> 'b t
    val iter : 'a t -> ('a -> unit io) -> unit io
    val close : 'a t -> unit
  end with type 'a io := 'a t
end

(** GraphQL schema signature *)
module type Schema = sig
  module Io : IO

  module StringMap : sig
    include Map.S with type key = string
    (* Map.S with type key = String.t *)
    exception Missing_key of key
    val find_exn : key -> 'a t -> 'a
    val find : key -> 'a t -> 'a option
  end

  type err

  type ctx

  (** {3 Base types } *)

  type schema

  type ('src) field

  type ('src) fast_field

  type subscription_field

  type ('src) typ

  type 'a enum_value

  (** {3 Constructors } *)

  val schema : ?mutation_name:string ->
               ?mutations:(unit) field list ->
               ?subscription_name:string ->
               ?subscriptions: subscription_field list ->
               ?query_name:string ->
               (unit) field list ->
               schema

  type deprecated =
    | NotDeprecated
    | Deprecated of string option

  val enum_value : ?doc:string ->
                   ?deprecated:deprecated ->
                   string ->
                   value:'a ->
                   'a enum_value

  val obj : ?doc:string ->
            string ->
            fields:(('src option) typ -> ('src) field list) ->
            ('src option) typ

  module Arg : sig
    type _ arg
    type _ arg_typ

    type (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | (::) : 'a arg * ('b, 'c) arg_list -> ('b, 'a -> 'c) arg_list

    val arg : ?doc:string ->
              string ->
              typ:'a arg_typ ->
              'a arg

    val arg' : ?doc:string ->
               string ->
               typ:'a option arg_typ ->
               default:'a ->
               'a arg

    val scalar : ?doc:string ->
                 string ->
                 coerce:(Graphql_parser.const_value -> ('a, string) result) ->
                 'a option arg_typ

    val enum : ?doc:string ->
               string ->
               values:'a enum_value list ->
               'a option arg_typ

    val obj : ?doc:string ->
              string ->
              fields:(('a option arg_typ) -> ('a, 'b) arg_list) ->
              coerce:'b ->
              'a option arg_typ

    (* Argument constructors *)
    val int : int option arg_typ
    val string : string option arg_typ
    val bool : bool option arg_typ
    val float : float option arg_typ
    val guid : string option arg_typ
    val list : 'a arg_typ -> 'a list option arg_typ
    val non_null : 'a option arg_typ -> 'a arg_typ
  end

  type variable_map = Graphql_parser.const_value StringMap.t
  type fragment_map = Graphql_parser.fragment StringMap.t
  type resolve_info = {
    ctx : ctx;
    field : Graphql_parser.field;
    fragments : fragment_map;
    variables : variable_map;
    operation : Graphql_parser.operation;
  }

  val fast_field : ?doc:string ->
              ?deprecated:deprecated ->
              string ->
              typ:('a) typ ->
              args:('a, 'b) Arg.arg_list ->
              resolve:(resolve_info -> 'src -> 'b) ->
              ('src) fast_field


  val field : ?doc:string ->
              ?deprecated:deprecated ->
              string ->
              typ:('a) typ ->
              args:('a, 'b) Arg.arg_list ->
              resolve:(resolve_info -> 'src -> 'b) ->
              ('src) field

  val io_field : ?doc:string ->
                 ?deprecated:deprecated ->
                 string ->
                 typ:('a) typ ->
                 args:(('a, err) result Io.t, 'b) Arg.arg_list ->
                 resolve:(resolve_info -> 'src -> 'b) ->
                 ('src) field

  val io_field_with_set_context : ?doc:string ->
                                  ?deprecated:deprecated ->
                                  string ->
                                  typ:('a) typ ->
                                  args:(('a, err) result Io.t, 'b) Arg.arg_list ->
                                  resolve:(((ctx -> ctx) -> ctx) ->  resolve_info -> 'src -> 'b) ->
                                  ('src) field

  val subscription_field : ?doc:string ->
                           ?deprecated:deprecated ->
                           string ->
                           typ:('out) typ ->
                           args:(('out Io.Stream.t, err) result Io.t, 'args) Arg.arg_list ->
                           resolve:(resolve_info -> 'args) ->
                           subscription_field

  val enum : ?doc:string ->
             string ->
             values:'a enum_value list ->
             ('a option) typ

  val scalar : ?doc:string ->
               string ->
               coerce:('a -> Yojson.Basic.json) ->
               ('a option) typ
    [@warning "-3"]

  val list : ('src) typ -> ('src list option) typ

  val non_null : ('src option) typ -> ('src) typ

  type ('a) abstract_value
  type ('a) abstract_typ = (('a) abstract_value option) typ

  val union : ?doc:string ->
              string ->
              ('a) abstract_typ

  type abstract_field
  val abstract_field : ?doc:string ->
                       ?deprecated:deprecated ->
                       string ->
                       typ:('a) typ ->
                       args:('a, _) Arg.arg_list ->
                       abstract_field

  val interface : ?doc:string ->
                  string ->
                  fields:(('a) abstract_typ -> abstract_field list) ->
                  ('a) abstract_typ

  val add_type : ('a) abstract_typ ->
                 ('src option) typ ->
                 'src -> ('a) abstract_value

  (** {3 Built-in scalars} *)

  val int    : (int option) typ
  val string : (string option) typ
  val guid   : (string option) typ
  val bool   : (bool option) typ
  val float  : (float option) typ

  type variables = (string * Graphql_parser.const_value) list

  type 'a response = ('a, Yojson.Basic.json) result
    [@warning "-3"]

  val prepare_schema : schema -> schema

  val execute : schema -> ctx -> ?variables:variables -> ?operation_name:string -> Graphql_parser.document -> [ `Response of Yojson.Basic.json | `Stream of Yojson.Basic.json response Io.Stream.t] response Io.t
    [@warning "-3"]
  (** [execute schema ctx variables doc] evaluates the [doc] against [schema]
      with the given context [ctx] and [variables]. *)
end
