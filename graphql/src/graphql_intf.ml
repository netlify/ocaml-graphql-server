module StringMap = struct
  include Map.Make (String)

  exception Missing_key of string

  let find_exn key t =
    try find key t with Not_found -> raise (Missing_key key)

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
  end
  with type 'a io := 'a t
end

(* Field_error signature *)
module type Field_error = sig
  type +'a io
  type t

  val message_of_field_error : t -> string io

  val extensions_of_field_error :
    t -> ((string * Yojson.Basic.t)) list option io
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

  type field_error

  (** {3 Base types } *)

  type 'ctx schema

  type ('ctx, 'src) field

  type 'ctx subscription_field

  type ('ctx, 'src) typ

  type 'a enum_value

  type directive_location =
    [ `Query
    | `Mutation
    | `Subscription
    | `Field
    | `Fragment_definition
    | `Fragment_spread
    | `Inline_fragment
    | `Variable_definition ]

  type directive_resolve = [ `Skip | `Include ]

  type directive

  (** {3 Constructors } *)

  val schema :
    ?mutation_name:string ->
    ?mutations:(('ctx, unit option) typ -> ('ctx, unit) field list) ->
    ?subscription_name:string ->
    ?subscriptions:(('ctx, unit option) typ -> 'ctx subscription_field list) ->
    ?query_name:string ->
    ?directives:directive list ->
    (('ctx, unit option) typ -> ('ctx, unit) field list) ->
    (* ('ctx, unit) field list -> *)
    'ctx schema

  type deprecated = NotDeprecated | Deprecated of string option

  val enum_value :
    ?doc:string ->
    ?deprecated:deprecated ->
    string ->
    value:'a ->
    'a enum_value

  val obj :
    ?doc:string ->
    string ->
    fields:(('ctx, 'src option) typ -> ('ctx, 'src) field list) ->
    ('ctx, 'src option) typ

  module Arg : sig
    type _ arg

    type _ arg_typ

    type (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | ( :: ) : 'a arg * ('b, 'c) arg_list -> ('b, 'a -> 'c) arg_list

    val arg : ?doc:string -> string -> typ:'a arg_typ -> 'a arg

    val arg' :
      ?doc:string ->
      string ->
      typ:'a option arg_typ ->
      default:Graphql_parser.const_value ->
      'a arg

    val scalar :
      ?doc:string ->
      string ->
      coerce:(Graphql_parser.const_value -> ('a, string) result) ->
      'a option arg_typ

    val enum :
      ?doc:string -> string -> values:'a enum_value list -> 'a option arg_typ

    val obj :
      ?doc:string ->
      string ->
      fields:('a option arg_typ -> ('a, 'b) arg_list) ->
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

   val eval_arglist : 'a 'b.
    Graphql_parser.const_value StringMap.t ->
      ?field_type:string ->
      field_name:string ->
      ('a, 'b) arg_list ->
      (string * Graphql_parser.value) list -> 'b -> ('a, string) result
  end

  type variable_map = Graphql_parser.const_value StringMap.t

  type fragment_map = Graphql_parser.fragment StringMap.t

  type path = [ `String of string | `Int of int ] list

  type 'ctx resolve_info = {
    ctx : 'ctx;
    field : Graphql_parser.field;
    fragments : fragment_map;
    variables : variable_map;
    operation : Graphql_parser.operation;
    path : path;
  }

  val field :
    ?doc:string ->
    ?deprecated:deprecated ->
    string ->
    typ:('ctx, 'a) typ ->
    args:('a, 'b) Arg.arg_list ->
    resolve:('ctx resolve_info -> 'src -> 'b) ->
    ('ctx, 'src) field

  val io_field :
    ?doc:string ->
    ?deprecated:deprecated ->
    string ->
    typ:('ctx, 'a) typ ->
    args:(('a, field_error) result Io.t, 'b) Arg.arg_list ->
    resolve:('ctx resolve_info -> 'src -> 'b) ->
    ('ctx, 'src) field

  val io_field_with_set_context :
    ?doc:string ->
    ?deprecated:deprecated ->
    string ->
    typ:('ctx, 'a) typ ->
    args:(('a, field_error) result Io.t, 'b) Arg.arg_list ->
    resolve:((('ctx -> 'ctx) -> 'ctx) -> 'ctx resolve_info -> 'src -> 'b) ->
    ('ctx, 'src) field

  val subscription_field :
    ?doc:string ->
    ?deprecated:deprecated ->
    string ->
    typ:('ctx, 'out) typ ->
    args:(('out Io.Stream.t, field_error) result Io.t, 'args) Arg.arg_list ->
    resolve:('ctx resolve_info -> 'args) ->
    'ctx subscription_field

  val directive :
    ?doc: string ->
    string ->
    locations:(directive_location list) ->
    args:(directive_resolve, 'a) Arg.arg_list ->
    resolve:'a ->
    directive

  val enum :
    ?doc:string -> string -> values:'a enum_value list -> ('ctx, 'a option) typ

  val scalar :
    (?doc:string ->
     string ->
     coerce:('a -> Yojson.Basic.t) ->
     ('ctx, 'a option) typ)

  val list : ('ctx, 'src) typ -> ('ctx, 'src list option) typ

  val non_null : ('ctx, 'src option) typ -> ('ctx, 'src) typ

  type ('ctx, 'a) abstract_value

  type ('ctx, 'a) abstract_typ = ('ctx, ('ctx, 'a) abstract_value option) typ

  val union : ?doc:string -> string -> ('ctx, 'a) abstract_typ

  type abstract_field

  val abstract_field :
    ?doc:string ->
    ?deprecated:deprecated ->
    string ->
    typ:(_, 'a) typ ->
    args:('a, _) Arg.arg_list ->
    abstract_field

  val interface :
    ?doc:string ->
    string ->
    fields:(('ctx, 'a) abstract_typ -> abstract_field list) ->
    ('ctx, 'a) abstract_typ

  val add_type :
    ('ctx, 'a) abstract_typ ->
    ('ctx, 'src option) typ ->
    'src ->
    ('ctx, 'a) abstract_value

  (** {3 Built-in scalars} *)

  val int : ('ctx, int option) typ

  val string : ('ctx, string option) typ

  val guid : ('ctx, string option) typ

  val bool : ('ctx, bool option) typ

  val float : ('ctx, float option) typ

  type variables = (string * Graphql_parser.const_value) list

  type 'a response = (('a, Yojson.Basic.t) result)

  val execute :
    ('ctx schema ->
     'ctx ->
     ?variables:variables ->
     ?operation_name:string ->
     Graphql_parser.document ->
     [ `Response of Yojson.Basic.t
     | `Stream of Yojson.Basic.t response Io.Stream.t ]
     response
     Io.t[@warning "-3"])
  (** [execute schema ctx variables doc] evaluates the [doc] against [schema]
      with the given context [ctx] and [variables]. *)

  val introspection_result : ('ctx schema -> Yojson.Basic.t)
end
