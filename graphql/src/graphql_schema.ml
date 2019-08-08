(* Helper modules *)
module List = struct
  include List
  let assoc_exn = assoc
  let assoc x ys = try Some (assoc_exn x ys) with Not_found -> None

  let find_exn = find
  let find cond xs = try Some (find_exn cond xs) with Not_found -> None

  module Result = struct
    let rec join ?(memo=[]) = function
      | [] -> Ok (List.rev memo)
      | (Error _ as err)::_ -> err
      | (Ok x)::xs -> join ~memo:(x::memo) xs

    let all f xs =
      List.map f xs |> join
  end
end

module Option = struct
  let map x ~f = match x with None -> None | Some y -> Some (f y)
end

(* IO *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Stream : sig
    type +'a io
    type 'a t

    val map : 'a t -> ('a -> 'b io) -> 'b t
    val iter : 'a t -> ('a -> unit io) -> unit io
    val close : 'a t -> unit
  end with type 'a io := 'a t
end

module type Err = sig
    type t
    val message_of_error : t -> string
    val extensions_of_error : t -> (string * Yojson.Basic.json [@warning "-3"]) list
end

module type Context = sig
    type t
end

(* Schema *)
module Make (Io : IO) (Err: Err) (Context: Context) = struct
  type err = Err.t
  type ctx = Context.t
  module Io = struct
    include Io

    let map x ~f = bind x (fun x' -> return (f x'))
    let ok x = Io.return (Ok x)
    let error x = Io.return (Error x)

    let rec all = function
      | [] -> Io.return []
      | x::xs ->
          bind (all xs) (fun xs' ->
            map x ~f:(fun x' -> x'::xs')
          )

    module Result = struct
      let bind x f = bind x (function Ok x' -> f x' | Error _ as err -> Io.return err)
      let map_error x ~f = map x ~f:(function Ok _ as ok -> ok | Error err -> Error (f err))
      let map x ~f = map x ~f:(function Ok x' -> Ok (f x') | Error _ as err -> err)
    end

    let rec map_s ?(memo=[]) f = function
      | [] -> Io.return (List.rev memo)
      | x::xs ->
          bind (f x) (fun x' -> map_s ~memo:(x'::memo) f xs)

    let map_p f xs = List.map f xs |> all

    module Infix = struct
      let (>>|) x f = map x ~f
      let (>>=?) = Result.bind
    end
  end

  module StringMap = struct
    include Map.Make(String)
    exception Missing_key of string
    let find_exn key t = try find key t with Not_found -> raise (Missing_key key)
    let find k t = try Some (find_exn k t) with Missing_key _ -> None
    (* let of_list l = *)
    (*   List.fold_left (fun acc (k,v) -> add k v acc) empty l *)
  end

  module StringSet = Set.Make(String)

  type variable_map = Graphql_parser.const_value StringMap.t

  type deprecated =
    | NotDeprecated
    | Deprecated of string option

  type 'a enum_value = {
    name       : string;
    doc        : string option;
    deprecated : deprecated;
    value      : 'a;
  }

  type json = Yojson.Basic.json [@warning "-3"]

  let enum_value ?doc ?(deprecated=NotDeprecated) name ~value =
    { name; doc; deprecated; value; }

  let id : 'a. 'a -> 'a = fun x -> x

  module Arg = struct
    open Rresult

    type _ arg_typ =
      | Scalar : {
          name   : string;
          doc    : string option;
          coerce : Graphql_parser.const_value -> ('a, string) result;
        } -> 'a option arg_typ
      | Object : {
          name   : string;
          doc    : string option;
          fields : ('a, 'b) arg_list Lazy.t;
          coerce : 'b;
        } -> 'a option arg_typ
      | Enum : {
          name   : string;
          doc    : string option;
          values : 'a enum_value list;
        } -> 'a option arg_typ
      | List : 'a arg_typ -> 'a list option arg_typ
      | NonNullable : 'a option arg_typ -> 'a arg_typ
    and _ arg =
      | Arg : {
          name : string;
          doc : string option;
          typ : 'a arg_typ;
        } -> 'a arg
      | DefaultArg : {
          name : string;
          doc : string option;
          typ : 'a option arg_typ;
          default : 'a;
        } -> 'a arg
    and (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | (::) : 'a arg * ('b, 'c) arg_list -> ('b, 'a -> 'c) arg_list

    let arg ?doc name ~typ =
      Arg { name; doc; typ }

    let arg' ?doc name ~typ ~default =
      DefaultArg { name; doc; typ; default }

    let scalar ?doc name ~coerce =
      Scalar { name; doc; coerce }

    let enum ?doc name ~values =
      Enum { name; doc; values }

    let obj ?doc name ~fields ~coerce =
      let rec o = Object { name; doc; fields = lazy (fields o); coerce } in
      o

    let rec string_of_const_value : Graphql_parser.const_value -> string = function
      | `Null -> "null"
      | `Int i -> string_of_int i
      | `Float f -> string_of_float f
      | `String s -> Printf.sprintf "\"%s\"" s
      | `Bool b -> string_of_bool b
      | `Enum e -> e
      | `List l ->
          let values = List.map (fun i -> string_of_const_value i) l in
          Printf.sprintf "[%s]" (String.concat ", " values)
      | `Assoc a ->
          let values =
            List.map
              (fun (k, v) ->
                Printf.sprintf "%s: %s" k (string_of_const_value v) )
              a
          in
          Printf.sprintf "{%s}" (String.concat ", " values)

    let rec string_of_arg_typ : type a. a arg_typ -> string = function
      | Scalar a -> a.name
      | Object a -> a.name
      | Enum a -> a.name
      | List a -> Printf.sprintf "[%s]" (string_of_arg_typ a)
      | NonNullable a -> Printf.sprintf "%s!" (string_of_arg_typ a)

    let eval_arg_error ?(field_type="field") ~field_name ~arg_name arg_typ value =
      let found_str =
        match value with
        | Some v -> Printf.sprintf "found %s" (string_of_const_value v)
        | None -> "but not provided"
      in
      Printf.sprintf "Argument `%s` of type `%s` expected on %s `%s`, %s."
        arg_name
        (string_of_arg_typ arg_typ)
        field_type
        field_name
        found_str

    (* Built-in argument types *)
    let int = Scalar {
      name = "Int";
      doc = None;
      coerce = function
        | `Int n -> Ok n
        | _ -> Error "Invalid int"
    }

    let string = Scalar {
      name = "String";
      doc = None;
      coerce = function
        | `String s -> Ok s
        | _ -> Error "Invalid string"
    }

    let float = Scalar {
      name = "Float";
      doc = None;
      coerce = function
        | `Float f -> Ok f
        | `Int n -> Ok (float_of_int n)
        | _ -> Error "Invalid float"
    }

    let bool = Scalar {
      name = "Boolean";
      doc = None;
      coerce = function
        | `Bool b -> Ok b
        | _ -> Error "Invalid boolean"
    }

    let guid = Scalar {
      name = "ID";
      doc = None;
      coerce = function
        | `String s -> Ok s
        | `Int n -> Ok (string_of_int n)
        | _ -> Error "Invalid ID"
    }

    let non_null typ = NonNullable typ
    let list typ = List typ

    let rec value_to_const_value variable_map = function
    | `Null -> `Null
    | `Int _ as i -> i
    | `Float _ as f -> f
    | `String _ as s -> s
    | `Bool _ as b -> b
    | `Enum _ as e -> e
    | `Variable v -> StringMap.find_exn v variable_map
    | `List xs -> `List (List.map (value_to_const_value variable_map) xs)
    | `Assoc props ->
        let props' = List.map (fun (name, value) -> name, value_to_const_value variable_map value) props in
        `Assoc props'

    let rec eval_arglist
      : type a b. variable_map
      -> ?field_type:string
      -> field_name:string
      -> (a, b) arg_list
      -> (string * Graphql_parser.value) list
      -> b
      -> (a, string) result =
      fun variable_map ?field_type ~field_name arglist key_values f ->
        match arglist with
        | [] -> Ok f
        | (DefaultArg arg)::arglist' ->
            let arglist'' = (Arg { name = arg.name; doc = arg.doc; typ = arg.typ })::arglist' in
            eval_arglist variable_map ?field_type ~field_name arglist'' key_values (function
              | None -> f arg.default
              | Some value -> f value
            )
        | (Arg arg)::arglist' ->
            try
              let value = List.assoc arg.name key_values in
              let const_value = Option.map value ~f:(value_to_const_value variable_map) in
              eval_arg variable_map ?field_type ~field_name ~arg_name:arg.name arg.typ const_value >>= fun coerced ->
              eval_arglist variable_map ?field_type ~field_name arglist' key_values (f coerced)
            with StringMap.Missing_key key -> Error (Format.sprintf "Missing variable `%s`" key)

    and eval_arg
      : type a. variable_map
      -> ?field_type:string
      -> field_name:string
      -> arg_name:string
      -> a arg_typ
      -> Graphql_parser.const_value option
      -> (a, string) result =
      fun variable_map ?field_type ~field_name ~arg_name typ value ->
        match (typ, value) with
        | NonNullable _, None -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ value)
        | NonNullable _, Some `Null -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ value)
        | Scalar _, None -> Ok None
        | Scalar _, Some `Null -> Ok None
        | Object _, None -> Ok None
        | Object _, Some `Null -> Ok None
        | List _, None -> Ok None
        | List _, Some `Null -> Ok None
        | Enum _, None -> Ok None
        | Enum _, Some `Null -> Ok None
        | Scalar s, Some value ->
          begin match (s.coerce value) with
          | Ok coerced -> Ok (Some coerced)
          | Error _ -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ (Some value))
          end
        | Object o, Some value ->
            begin match value with
            | `Assoc props ->
                let props' = (props :> (string * Graphql_parser.value) list) in
                eval_arglist variable_map ?field_type ~field_name Lazy.(force o.fields) props' o.coerce >>| fun coerced ->
                Some coerced
            | _ -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ (Some value))
            end
      | List typ, Some value ->
            begin match value with
            | `List values ->
                let option_values = List.map (fun x -> Some x) values in
                List.Result.all (eval_arg variable_map ?field_type ~field_name ~arg_name typ) option_values >>| fun coerced ->
                Some coerced
            | value -> eval_arg variable_map ?field_type ~field_name ~arg_name typ (Some value) >>| fun coerced ->
                (Some [coerced] : a)
            end
        | NonNullable typ, value ->
            eval_arg variable_map ?field_type ~field_name ~arg_name typ value >>= (function
            | Some value -> Ok value
            | None -> Error (eval_arg_error ?field_type ~field_name ~arg_name typ None))
        | Enum e, Some value ->
            begin match value with
            | `Enum v
            | `String v ->
                begin match List.find (fun enum_value -> enum_value.name = v) e.values with
                | Some enum_value -> Ok (Some enum_value.value)
                | None -> Error (Printf.sprintf "Invalid enum value for argument `%s` on field `%s`" arg_name field_name)
                end
            | _ -> Error (Printf.sprintf "Expected enum for argument `%s` on field `%s`" arg_name field_name)
            end
  end

  (* Schema data types *)
  type 'a scalar = {
    name   : string;
    doc    : string option;
    coerce : 'a -> json;
  }

  type 'a enum = {
    name    : string;
    doc     : string option;
    values  : 'a enum_value list;
  }

  type fragment_map = Graphql_parser.fragment StringMap.t
  type resolve_info = {
    ctx : ctx;
    field : Graphql_parser.field;
    fragments : fragment_map;
    variables : variable_map;
    operation : Graphql_parser.operation;
  }

  type 'src obj = {
    name   : string;
    doc    : string option;
    fields : 'src field list Lazy.t;
    fast_fields: 'src fast_field list Lazy.t;
    fields_memo : (string, ('src field)) Hashtbl.t Lazy.t;
    fast_fields_memo : (string, ('src fast_field)) Hashtbl.t Lazy.t;
    abstracts : abstract list ref;
  }
  and _ field =
    Field : {
      name       : string;
      doc        : string option;
      deprecated : deprecated;
      typ        : 'out typ;
      args       : ('a, 'args) Arg.arg_list;
      resolve    : [`Resolve of resolve_info -> 'src -> 'args
                   | `Resolve_with_set_child_context of ((ctx -> ctx) -> ctx) -> resolve_info -> 'src -> 'args ];
      lift       : 'a -> ('out, err) result Io.t;
    } -> 'src field
  and _ fast_field =
    FastField : {
      name       : string;
      doc        : string option;
      deprecated : deprecated;
      typ        : 'out typ;
      args       : ('a, 'args) Arg.arg_list;
      resolve    : [`Resolve of  resolve_info -> 'src -> 'args
                   | `Resolve_with_set_child_context of ((ctx -> ctx) -> ctx) -> resolve_info -> 'src -> 'args ];
      lift       : 'a -> ('out, err) result;
    } -> 'src fast_field
  and _ typ =
    | Object      : 'src obj -> 'src option typ
    | List        : 'src typ -> ('src list option) typ
    | NonNullable : ('src option) typ -> 'src typ
    | Scalar      : 'src scalar -> ('src option) typ
    | Enum        : 'src enum -> ('src option) typ
    | Abstract    : abstract -> ('a abstract_value option) typ
  and any_typ =
    | AnyTyp : _ typ -> any_typ
    | AnyArgTyp : _ Arg.arg_typ -> any_typ
  and abstract = {
    name   : string;
    doc    : string option;
    kind   : [`Union | `Interface of abstract_field list Lazy.t];
    mutable types  : any_typ list;
  }
  and abstract_field =
    AbstractField : _ field -> abstract_field
  and 'a abstract_value =
    AbstractValue : ('src option) typ * 'src -> 'a abstract_value

  type subscription_field =
    SubscriptionField : {
      name       : string;
      doc        : string option;
      deprecated : deprecated;
      typ        : 'out typ;
      args       : (('out Io.Stream.t, err) result Io.t, 'args) Arg.arg_list;
      resolve    : resolve_info -> 'args;
    } -> subscription_field

  type subscription_obj = {
    name   : string;
    doc    : string option;
    fields : subscription_field list;
  }

  type 'a abstract_typ = ('a abstract_value option) typ

  type directive_location = [
    | `Query
    | `Mutation
    | `Subscription
    | `Field
    | `Fragment_definition
    | `Fragment_spread
    | `Inline_fragment
    | `Variable_definition
    ]

  type directive =
    Directive : {
      name       : string;
      doc        : string option;
      locations  : directive_location list;
      args       : ([ `Skip | `Include ], 'args) Arg.arg_list;
      resolve    : 'args;
    } -> directive

  type schema = {
    query : unit obj;
    mutation : unit obj option;
    subscription : subscription_obj option;

  }

  let schema ?(mutation_name="mutation")
             ?mutations
             ?(subscription_name="subscription")
             ?subscriptions
             ?(query_name="query")
             fields = {
    query = {
      name = query_name;
      doc = None;
      abstracts = ref [];
      fields = lazy fields;
      fast_fields = lazy [];
      fast_fields_memo = (lazy (Hashtbl.create 1));
      fields_memo = (lazy (List.fold_left (fun acc (field: ('a) field) ->
                                           Hashtbl.add acc (match field with | Field f -> f.name) field;
                                           acc)
                                           (Hashtbl.create (List.length fields))
                                           fields));
    };
    mutation = Option.map mutations ~f:(fun fields ->
      {
        name = mutation_name;
        doc = None;
        abstracts = ref [];
        fast_fields = lazy [];
        fields = lazy fields;
        fast_fields_memo = (lazy (Hashtbl.create 1));
        fields_memo = (lazy (List.fold_left (fun acc (field: ('a) field) ->
                                           Hashtbl.add acc (match field with | Field f -> f.name) field;
                                           acc)
                                           (Hashtbl.create (List.length fields))
                                           fields));

      }
    );
    subscription = Option.map subscriptions ~f:(fun fields ->
      {
        name = subscription_name;
        doc = None;
        fields;
      }
    )
  }

  (* Constructor functions *)
  let obj ?doc name ~fields =
    let rec o = Object { name; doc; fields = resolved_fields; fast_fields = lazy []; fields_memo = fields_memo;       fast_fields_memo = (lazy (Hashtbl.create 1)); abstracts = ref [];} and
        resolved_fields = lazy (fields o) and
        fields_memo = (lazy (
                           print_endline ("Forcing fields for " ^ name);
                           (List.fold_left (fun acc (field: ('a) field) ->
                                           Hashtbl.add acc (match field with | Field f -> f.name) field;
                                           acc)
                                           (Hashtbl.create (List.length (Lazy.force resolved_fields)))
                                           (Lazy.force resolved_fields)))) in
    o

  let fast_obj ?doc name ~fields =
    let rec o = Object { name; doc; fields = lazy []; fast_fields = resolved_fields; fast_fields_memo = fast_fields_memo; fields_memo = lazy (Hashtbl.create 1); abstracts = ref [];} and
        resolved_fields = lazy (fields o) and
        fast_fields_memo = (lazy (
                           print_endline ("Forcing fields for " ^ name);
                           (List.fold_left (fun acc (field: ('a) fast_field) ->
                                           Hashtbl.add acc (match field with | FastField f -> f.name) field;
                                           acc)
                                           (Hashtbl.create (List.length (Lazy.force resolved_fields)))
                                           (Lazy.force resolved_fields)))) in
    o


  let field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    Field { name; doc; deprecated; typ; args; resolve = `Resolve resolve; lift = Io.ok }

  let io_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    Field { name; doc; deprecated; typ; args; resolve = `Resolve resolve; lift = id }

  let io_field_with_set_context ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    Field { name; doc; deprecated; typ; args; resolve = `Resolve_with_set_child_context resolve; lift = id }

  let fast_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    FastField { name; doc; deprecated; typ; args; resolve = `Resolve resolve; lift = (fun x -> Ok x)}

  let abstract_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args =
    AbstractField (Field { lift = Io.ok; name; doc; deprecated; typ; args; resolve = Obj.magic () })

  let subscription_field ?doc ?(deprecated=NotDeprecated) name ~typ ~args ~resolve =
    SubscriptionField { name; doc; deprecated; typ; args; resolve }

  let enum ?doc name ~values =
    Enum { name; doc; values }

  let scalar ?doc name ~coerce =
    Scalar { name; doc; coerce }

  let list typ =
    List typ

  let non_null typ =
    NonNullable typ

  let union ?doc name =
    Abstract { name; doc; types = []; kind = `Union }

  let interface ?doc name ~fields =
    let rec i = Abstract { name; doc; types = []; kind = `Interface (lazy (fields i)) } in
    i

  let add_type abstract_typ typ =
    match (abstract_typ, typ) with
    | Abstract a, Object o ->
        (* TODO add subtype check here *)
        a.types <- (AnyTyp typ)::a.types;
        o.abstracts := a :: !(o.abstracts);
        fun src -> AbstractValue (typ, src)
    | _ ->
        invalid_arg "Arguments must be Interface/Union and Object"

  let obj_of_subscription_obj {name; doc; fields} =
    let fields = List.map
      (fun (SubscriptionField {name; doc; deprecated; typ; args; resolve}) ->
        Field { lift = Obj.magic (); name; doc; deprecated; typ; args; resolve = `Resolve (fun ctx () -> resolve ctx) })
      fields
    in
    { name; doc; abstracts = ref []; fields = lazy fields; fast_fields = lazy []; fast_fields_memo = lazy (Hashtbl.create 1); fields_memo = (lazy (List.fold_left (fun acc (field: ('a) field) ->
                                           Hashtbl.add acc (match field with | Field f -> f.name) field;
                                           acc)
                                           (Hashtbl.create (List.length fields))
                                           fields));
}

  (* Built-in scalars *)
  let int : (int option) typ = Scalar {
    name   = "Int";
    doc    = None;
    coerce = fun i -> `Int i;
  }

  let string : (string option) typ = Scalar {
    name   = "String";
    doc    = None;
    coerce = fun s ->`String s;
  }

  let bool : (bool option) typ = Scalar {
    name   = "Boolean";
    doc    = None;
    coerce = fun b -> `Bool b;
  }

  let float : (float option) typ = Scalar {
    name   = "Float";
    doc    = None;
    coerce = fun f -> `Float f;
  }

  let guid : (string option) typ = Scalar {
    name   = "ID";
    doc    = None;
    coerce = fun x -> `String x;
  }

  (* Mandatory directives: skip and include *)
  let skip_directive = Directive {
    name = "skip";
    doc = Some "Directs the executor to skip this field or fragment when the `if` argument is true.";
    locations = [`Field; `Fragment_spread; `Inline_fragment];
    args = Arg.[
      arg "if" ~doc:"Skipped when true." ~typ:(non_null bool)
    ];
    resolve = function
      | true -> `Skip
      | false -> `Include
  }

  let include_directive = Directive {
    name = "include";
    doc = Some "Directs the executor to include this field or fragment only when the `if` argument is true.";
    locations = [`Field; `Fragment_spread; `Inline_fragment];
    args = Arg.[
      arg "if" ~doc:"Included when true." ~typ:(non_null bool)
    ];
    resolve = function
      | true -> `Include
      | false -> `Skip
  }

module Introspection = struct
  (* any_typ, any_field and any_arg hide type parameters to avoid scope escaping errors *)
  type any_field =
    | AnyField : (_) field -> any_field
    | AnyArgField : _ Arg.arg -> any_field
  type any_arg = AnyArg : _ Arg.arg -> any_arg
  type any_enum_value = AnyEnumValue : _ enum_value -> any_enum_value

  let ok = (fun x -> (Ok x))

  let unless_visited memo name f =
    if StringMap.mem name memo then
      memo
    else
      f memo

  (* Extracts all types contained in a single type *)
  let rec types : type src. ?memo: any_typ StringMap.t -> (src) typ -> any_typ StringMap.t = fun ?(memo=StringMap.empty) typ ->
    match typ with
    | List typ -> types ~memo typ
    | NonNullable typ -> types ~memo typ
    | Scalar s as scalar ->
        unless_visited memo s.name (fun memo' ->
          StringMap.add s.name (AnyTyp scalar) memo'
        )
    | Enum e as enum ->
        unless_visited memo e.name (fun memo' ->
          StringMap.add e.name (AnyTyp enum) memo'
        )
    | Object o as obj ->
        unless_visited memo o.name (fun memo' ->
          let memo'' = StringMap.add o.name (AnyTyp obj) memo' in
          let reducer = fun memo (Field f) ->
            let memo' = types ~memo f.typ in
            arg_list_types memo' f.args
          in
          let memo' = List.fold_left reducer memo'' (Lazy.force o.fields) in
          let abstracts_reducer = fun memo a ->
            types ~memo (Abstract a)
          in
          List.fold_left abstracts_reducer memo' !(o.abstracts)
        )
   | Abstract a as abstract ->
      unless_visited memo a.name (fun memo' ->
        let memo'' = StringMap.add a.name (AnyTyp abstract) memo' in
        List.fold_left (fun memo typ -> match typ with
          | AnyTyp typ -> types ~memo typ
          | AnyArgTyp _ -> failwith "Abstracts can't have argument types")
          memo'' a.types
      )

  and arg_types : type a. any_typ StringMap.t -> a Arg.arg_typ -> any_typ StringMap.t = fun memo argtyp ->
    match argtyp with
    | Arg.List typ -> arg_types memo typ
    | Arg.NonNullable typ -> arg_types memo typ
    | Arg.Scalar s as scalar ->
        unless_visited memo s.name (fun memo' ->
          StringMap.add s.name (AnyArgTyp scalar) memo'
        )
    | Arg.Enum e as enum ->
        unless_visited memo e.name (fun memo' ->
          StringMap.add e.name (AnyArgTyp enum) memo'
        )
    | Arg.Object o as obj ->
        unless_visited memo o.name (fun memo' ->
          let memo'' = StringMap.add o.name (AnyArgTyp obj) memo' in
          arg_list_types memo'' Lazy.(force o.fields)
        )
  and arg_list_types : type a b. any_typ StringMap.t -> (a, b) Arg.arg_list -> any_typ StringMap.t = fun memo arglist ->
    let open Arg in
    match arglist with
    | [] -> memo
    | arg::args ->
        let memo' = match arg with
        | Arg a -> arg_types memo a.typ
        | DefaultArg a -> arg_types memo a.typ
        in arg_list_types memo' args

  let rec args_to_list : type a b. ?memo:any_arg list -> (a, b) Arg.arg_list -> any_arg list = fun ?memo:(memo=[]) arglist ->
    let open Arg in
    match arglist with
    | [] ->
        memo
    | arg::args ->
        let memo' = List.cons (AnyArg arg) memo in
        args_to_list ~memo:memo' args

  (* let no_abstracts = ref [] *)

  let __type_kind = Enum {
    name = "__TypeKind";
    doc = None;
    values = [
      {
        name="SCALAR";
        doc=None;
        deprecated=NotDeprecated;
        value=`Scalar;
      };
      {
        name="OBJECT";
        doc=None;
        deprecated=NotDeprecated;
        value=`Object;
      };
      {
        name="INTERFACE";
        doc=None;
        deprecated=NotDeprecated;
        value=`Interface;
      };
      {
        name="UNION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Union;
      };
      {
        name="ENUM";
        doc=None;
        deprecated=NotDeprecated;
        value=`Enum;
      };
      {
        name="INPUT_OBJECT";
        doc=None;
        deprecated=NotDeprecated;
        value=`InputObject;
      };
      {
        name="LIST";
        doc=None;
        deprecated=NotDeprecated;
        value=`List;
      };
      {
        name="NON_NULL";
        doc=None;
        deprecated=NotDeprecated;
        value=`NonNull;
      };
    ]
  }

  let __enum_value : (any_enum_value option) typ = fast_obj  "__EnumValue" ~fields: (fun _ ->
    [
      FastField {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyEnumValue enum_value) -> enum_value.name);
      };
      FastField {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyEnumValue enum_value) -> enum_value.doc);
      };
      FastField {
        name = "isDeprecated";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable bool;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyEnumValue enum_value) -> enum_value.deprecated <> NotDeprecated);
      };
      FastField {
        name = "deprecationReason";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyEnumValue enum_value) ->
          match enum_value.deprecated with
          | Deprecated reason -> reason
          | NotDeprecated -> None)
      }
    ])

  let rec __input_value : (any_arg option) typ Lazy.t = lazy (fast_obj
    "__InputValue"
    ~fields: (fun _ -> [
      FastField {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyArg arg) -> match arg with
          | Arg.DefaultArg a -> a.name
          | Arg.Arg a -> a.name)
      };
      FastField {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyArg arg) -> match arg with
          | Arg.DefaultArg a -> a.doc
          | Arg.Arg a -> a.doc)
      };
      FastField {
        name = "type";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (Lazy.force __type);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyArg arg) -> match arg with
          | Arg.DefaultArg a -> AnyArgTyp a.typ
          | Arg.Arg a -> AnyArgTyp a.typ)
      };
      FastField {
        name = "defaultValue";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (AnyArg _) -> None)
      }
    ]))


  and __type : (any_typ option) typ Lazy.t = lazy (fast_obj
    "__Type"
    ~fields: (fun _ -> [
      FastField {
        name = "kind";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable __type_kind;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyTyp (Object _) -> `Object
          | AnyTyp (Abstract { kind = `Union; _ }) -> `Union
          | AnyTyp (Abstract { kind = `Interface _; _ }) -> `Interface
          | AnyTyp (List _) -> `List
          | AnyTyp (Scalar _) -> `Scalar
          | AnyTyp (Enum _) -> `Enum
          | AnyTyp (NonNullable _) -> `NonNull
          | AnyArgTyp (Arg.Object _) -> `InputObject
          | AnyArgTyp (Arg.List _) -> `List
          | AnyArgTyp (Arg.Scalar _) -> `Scalar
          | AnyArgTyp (Arg.Enum _) -> `Enum
          | AnyArgTyp (Arg.NonNullable _) -> `NonNull)
      };
      FastField {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyTyp (Object o) -> Some o.name
          | AnyTyp (Scalar s) -> Some s.name
          | AnyTyp (Enum e) -> Some e.name
          | AnyTyp (Abstract a) -> Some a.name
          | AnyArgTyp (Arg.Object o) -> Some o.name
          | AnyArgTyp (Arg.Scalar s) -> Some s.name
          | AnyArgTyp (Arg.Enum e) -> Some e.name
          | _ -> None);
      };
      FastField {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyTyp (Object o) -> o.doc
          | AnyTyp (Scalar s) -> s.doc
          | AnyTyp (Enum e) -> e.doc
          | AnyTyp (Abstract a) -> a.doc
          | AnyArgTyp (Arg.Object o) -> o.doc
          | AnyArgTyp (Arg.Scalar s) -> s.doc
          | AnyArgTyp (Arg.Enum e) -> e.doc
          | _ -> None)
      };
      FastField {
        name = "fields";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable (Lazy.force __field));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t ->
                            let res = match t with
          | AnyTyp (Object o) ->
              Some (List.map (fun f -> AnyField f) (Lazy.force o.fields))
          | AnyTyp (Abstract { kind = `Interface fields; _ }) ->
              Some (List.map (fun (AbstractField f) -> AnyField f) (Lazy.force fields))
          | AnyArgTyp (Arg.Object o) ->
              let arg_list = args_to_list Lazy.(force o.fields) in
              Some (List.map (fun (AnyArg f) -> AnyArgField f) arg_list)
          | _ -> None in

                           res)
      };
      FastField {
        name = "interfaces";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable (Lazy.force __type));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyTyp (Object o) ->
              let interfaces = List.filter (function | { kind = `Interface _; _} -> true | _ -> false) !(o.abstracts) in
              Some (List.map (fun i -> AnyTyp (Abstract i)) interfaces)
          | _ -> None)
      };
      FastField {
        name = "possibleTypes";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable (Lazy.force __type));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyTyp (Abstract a) ->
              Some a.types
          | _ -> None)
      };
      FastField {
        name = "ofType";
        doc = None;
        deprecated = NotDeprecated;
        typ = (Lazy.force __type);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyTyp (NonNullable typ) -> Some (AnyTyp typ)
          | AnyTyp (List typ) -> Some (AnyTyp typ)
          | AnyArgTyp (Arg.NonNullable typ) -> Some (AnyArgTyp typ)
          | AnyArgTyp (Arg.List typ) -> Some (AnyArgTyp typ)
          | _ -> None)
      };
      FastField {
        name = "inputFields";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable (Lazy.force __input_value));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyArgTyp (Arg.Object o) ->
              Some (args_to_list Lazy.(force o.fields))
          | _ -> None)
      };
      FastField {
        name = "enumValues";
        doc = None;
        deprecated = NotDeprecated;
        typ = List (NonNullable __enum_value);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ t -> match t with
          | AnyTyp (Enum e) -> Some (List.map (fun x -> AnyEnumValue x) e.values)
          | AnyArgTyp (Arg.Enum e) -> Some (List.map (fun x -> AnyEnumValue x) e.values)
          | _      -> None)
      }
    ]))


  and __field : (any_field option) typ Lazy.t = lazy (fast_obj
    "__Field"
    ~fields: (fun _ -> [
      FastField {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ f -> match f with
          | AnyField (Field f) -> f.name
          | AnyArgField (Arg.Arg a) -> a.name
          | AnyArgField (Arg.DefaultArg a) -> a.name)
      };
      FastField {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ f -> match f with
          | AnyField (Field f) -> f.doc
          | AnyArgField (Arg.Arg a) -> a.doc
          | AnyArgField (Arg.DefaultArg a) -> a.doc)
      };
      FastField {
        name = "args";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable (Lazy.force __input_value)));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ f ->
                            let res =
                            match f with
          | AnyField (Field f) -> args_to_list f.args
          | AnyArgField _ -> [] in

        res)
      };
      FastField {
        name = "type";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (Lazy.force __type);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ f -> match f with
          | AnyField (Field f) -> AnyTyp f.typ
          | AnyArgField (Arg.Arg a) -> AnyArgTyp a.typ
          | AnyArgField (Arg.DefaultArg a) -> AnyArgTyp a.typ)
      };
      FastField {
        name = "isDeprecated";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable bool;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ f -> match f with
          | AnyField (Field { deprecated = Deprecated _; _ }) -> true
          | _ -> false)
      };
      FastField {
        name = "deprecationReason";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ f -> match f with
          | AnyField (Field { deprecated = Deprecated reason; _ }) -> reason
          | _ -> None)
      }
    ]))


  let __directive_location = Enum {
    name = "__DirectiveLocation";
    doc = None;
    values = [
      {
        name="QUERY";
        doc=None;
        deprecated=NotDeprecated;
        value=`Query;
      };
      {
        name="MUTATION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Mutation;
      };
      {
        name="SUBSCRIPTION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Subscription;
      };
      {
        name="FIELD";
        doc=None;
        deprecated=NotDeprecated;
        value=`Field;
      };
      {
        name="FRAGMENT_DEFINITION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Fragment_definition;
      };
      {
        name="FRAGMENT_SPREAD";
        doc=None;
        deprecated=NotDeprecated;
        value=`Fragment_spread;
      };
      {
        name="INLINE_FRAGMENT";
        doc=None;
        deprecated=NotDeprecated;
        value=`Inline_fragment;
      };
      {
        name="VARIABLE_DEFINITION";
        doc=None;
        deprecated=NotDeprecated;
        value=`Variable_definition;
      };
    ]
  }

  let __directive = fast_obj
    "__Directive"
    ~fields: (fun _ -> [
      FastField {
        name = "name";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (Directive d) -> d.name)
      };
      FastField {
        name = "description";
        doc = None;
        deprecated = NotDeprecated;
        typ = string;
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (Directive d) -> d.doc)
      };
      FastField {
        name = "locations";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __directive_location));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (Directive d) -> d.locations)
      };
      FastField {
        name = "args";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable (Lazy.force __input_value)));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (Directive d) -> args_to_list d.args)
      }
    ])

  let __schema_fields: (schema * any_typ StringMap.t) fast_field list = [
      FastField {
        name = "types";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable (Lazy.force __type)));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (s, types) ->
          (* let types = List.fold_left *)
          (*   (fun memo op -> *)
          (*     match op with *)
          (*     | None -> memo *)
          (*     | Some op -> types ~memo (Object op)) *)
          (*   StringMap.empty *)
          (*   [Some s.query; s.mutation; Option.map s.subscription ~f:obj_of_subscription_obj] *)
          (* in *)
          (List.map (fun (_, vs) -> vs) (StringMap.bindings types)))
      };
      FastField {
        name = "queryType";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (Lazy.force __type);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (s, _) -> AnyTyp (Object s.query))
      };
      FastField {
        name = "mutationType";
        doc = None;
        deprecated = NotDeprecated;
        typ = (Lazy.force __type);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (s, _) -> Option.map s.mutation ~f:(fun mut -> AnyTyp (Object mut)))
      };
      FastField {
        name = "subscriptionType";
        doc = None;
        deprecated = NotDeprecated;
        typ = (Lazy.force __type);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ (s, _) ->
          Option.map s.subscription ~f:(fun subs -> AnyTyp (Object (obj_of_subscription_obj subs))))
      };
      FastField {
        name = "directives";
        doc = None;
        deprecated = NotDeprecated;
        typ = NonNullable (List (NonNullable __directive));
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ _ -> [])
      };
      FastField {
        name = "subscriptionType";
        doc = None;
        deprecated = NotDeprecated;
        typ = (Lazy.force __type);
        args = Arg.[];
        lift = ok;
        resolve = `Resolve (fun _ _ -> None)
      }
    ]

 let __schema : ((schema * any_typ StringMap.t) option) typ = fast_obj
    "__Schema"
    ~fields: (fun _ -> __schema_fields)

  let add_schema_field s =
    let types = List.fold_left
      (fun memo op ->
        match op with
        | None -> memo
        | Some op -> types ~memo (Object op))
      StringMap.empty
      [Some s.query; s.mutation; Option.map s.subscription ~f:obj_of_subscription_obj]
    in

    let schema_field = FastField {
      name = "__schema";
      doc = None;
      deprecated = NotDeprecated;
      typ = NonNullable __schema;
      args = Arg.[];
      lift = ok;
      resolve = `Resolve (fun _ _ -> (s, types))
    } in
    (* let fields = schema_field::(Lazy.force s.query.fields) in *)
    (* let fields_memo = (lazy (List.fold_left (fun acc (field: ('a) field) -> *)
    (*                                        Hashtbl.add acc (match field with | Field f -> f.name) field; *)
    (*                                        acc) *)
    (*                                        (Hashtbl.create (List.length fields)) *)
    (*                                        fields)) in *)
    (* { s with query = { s.query with fields = lazy fields; fields_memo = fields_memo } } *)
    let fast_fields_memo = Hashtbl.create 1 in
    Hashtbl.add fast_fields_memo "__schema" schema_field;
    { s with query = { s.query with fast_fields = lazy [schema_field]; fast_fields_memo = lazy fast_fields_memo } }
end

  (* Execution *)
  type variables = (string * Graphql_parser.const_value) list
  type execution_order = Serial | Parallel
  type execution_context = {
    variables : variable_map;
    fragments : fragment_map;
    ctx       : ctx;
    operation : Graphql_parser.operation;
  }

  type path = [`String of string | `Int of int] list
  type error = err * path

  type resolve_error = [
    | `Resolve_error of error
    | `Argument_error of string
    | `Validation_error of string
  ]

  type execute_error = [
    resolve_error
    | `Mutations_not_configured
    | `Subscriptions_not_configured
    | `No_operation_found
    | `Operation_name_required
    | `Operation_not_found
  ]

  type 'a response = ('a, json) result

  let matches_type_condition type_condition (obj : ('src) obj) =
    obj.name = type_condition ||
      List.exists (fun (abstract : abstract) -> abstract.name = type_condition) !(obj.abstracts)

  let rec should_include_field ctx (directives : Graphql_parser.directive list) =
    match directives with
    | [] -> Ok true
    | { name = "skip"; arguments }::rest ->
      eval_directive ctx skip_directive arguments rest
    | { name = "include"; arguments }::rest ->
      eval_directive ctx include_directive arguments rest
    | { name; _ }::_ ->
        let err = Format.sprintf "Unknown directive: %s" name in
        Error err

  and eval_directive ctx (Directive { name; args; resolve; _ }) arguments rest =
    let open Rresult in
    Arg.eval_arglist ctx.variables ~field_type:"directive" ~field_name:name args arguments resolve >>= function
      | `Skip -> Ok false
      | `Include -> should_include_field ctx rest

  let collect_fields_memo = Hashtbl.create(2000)

  let rec collect_fields : execution_context -> ('src) obj -> Graphql_parser.selection list -> (Graphql_parser.field list, string) result =
    fun ctx obj fields ->
    match (Hashtbl.find_opt collect_fields_memo (obj.name, fields)) with
    | Some x -> x
    | None ->
    let open Rresult in
    let res = List.map (function
    | Graphql_parser.Field field ->
        should_include_field ctx field.directives >>| fun include_field ->
          if include_field then [field] else []
    | Graphql_parser.FragmentSpread spread ->
        begin match StringMap.find spread.name ctx.fragments with
        | Some { directives; type_condition; selection_set; _ }
          when matches_type_condition type_condition obj ->
          should_include_field ctx directives >>= fun include_field ->
            if include_field then
              collect_fields ctx obj selection_set
            else Ok []
        | _ -> Ok []
        end
    | Graphql_parser.InlineFragment fragment ->
        let matches_type_condition = match fragment.type_condition with
          | None -> true
          | Some condition -> matches_type_condition condition obj
        in
        if matches_type_condition then
          should_include_field ctx fragment.directives >>= fun include_field ->
            if include_field then
              collect_fields ctx obj fragment.selection_set
            else Ok []
        else
          Ok []
    ) fields
    |> List.Result.join
              |> Rresult.R.map List.concat in
    Hashtbl.add collect_fields_memo (obj.name, fields) res;
    res


  let alias_or_name : Graphql_parser.field -> string = fun field ->
    match field.alias with
    | Some alias -> alias
    | None -> field.name

  let field_from_object : ('src) obj -> string -> ('src) field option =
    (fun obj field_name ->
     Hashtbl.find_opt (Lazy.force obj.fields_memo) field_name)

  let fast_field_from_object : ('src) obj -> string -> ('src) fast_field option =
    (fun obj field_name ->
     Hashtbl.find_opt (Lazy.force obj.fast_fields_memo) field_name)

  let field_from_subscription_object = fun obj field_name ->
    List.find (fun (SubscriptionField field) -> field.name = field_name) obj.fields

  let coerce_or_null : 'a option -> ('a -> (json * error list, 'b) result Io.t) -> (json * error list, 'b) result Io.t =
    fun src f ->
      match src with
      | None -> Io.ok (`Null, [])
      | Some src' -> f src'

  let map_fields_with_order = function
    | Serial -> Io.map_s ~memo:[]
    | Parallel -> Io.map_p

  let error_to_json ?path ?extensions  msg =
    let props = match path with
    | Some path -> ["path", `List (List.rev path :> json list)]
    | None -> []
    in
    let extensionProps = match extensions with
      | None
      | Some [] -> []
      | Some extensions -> ["extensions", `Assoc extensions]
    in
    (`Assoc (("message", `String msg)::(List.concat [props; extensionProps])) : json)

  let error_response ?path ?data ?extensions msg =
    let errors = "errors", `List [
      error_to_json ?path ?extensions msg
    ]
    in
    let data = match data with
    | None -> []
    | Some data -> ["data", data]
    in
    `Assoc (errors :: data)

  let rec present : type src. execution_context -> src -> Graphql_parser.field -> (src) typ -> path -> (json * error list, [> resolve_error]) result Io.t =
    fun ctx src query_field typ path ->
      match typ with
      | Scalar s -> (match src with
                    | None -> Io.ok (`Null, [])
                    | Some src' -> Io.ok (s.coerce src', []))
      | List t ->
          (match src with
           | None -> Io.ok (`Null, [])
           | Some src' ->
            List.mapi (fun i x -> present ctx x query_field t ((`Int i)::path)) src'
            |> Io.all
            |> Io.map ~f:List.Result.join
            |> Io.Result.map ~f:(fun xs -> (`List (List.map fst xs), List.map snd xs |> List.concat))
          )
      | NonNullable t -> present ctx (Some src) query_field t path
      | Object o ->
         (match src with
          | None -> Io.ok (`Null, [])
          | Some src' ->
            match collect_fields ctx o query_field.selection_set with
            | Ok fields -> resolve_fields ctx src' o fields path
            | Error e -> Io.error (`Argument_error e))
      | Enum e ->
         (match src with
          | None -> Io.ok (`Null, [])
          | Some src' ->
            match List.find (fun enum_value -> src' = enum_value.value) e.values with
            | Some enum_value -> Io.ok (`String enum_value.name, [])
            | None -> Io.ok (`Null, [])
          )
      | Abstract _ ->
          (match src with
          | None -> Io.ok (`Null, [])
          | Some (AbstractValue (typ', src')) ->
            present ctx (Some src') query_field typ' path
          )

  and resolve_field : type src. execution_context -> src -> Graphql_parser.field -> (src) field -> path -> ((string * json) * error list, [> resolve_error]) result Io.t =
    fun ctx src query_field (Field field) path ->
      let open Io.Infix in
      let name = alias_or_name query_field in
      let path' = (`String name)::path in
      let resolve_info = {
        ctx = ctx.ctx;
        field = query_field;
        fragments = ctx.fragments;
        variables = ctx.variables;
        operation = ctx.operation;
      } in
      let ctx_ref = ref ctx.ctx in
      let resolver = match field.resolve with
      | `Resolve_with_set_child_context resolve ->
         let set_child_context = (fun f ->
           let new_ctx = (f !ctx_ref) in
           ctx_ref := new_ctx;
           new_ctx) in
         resolve set_child_context resolve_info src
      | `Resolve resolve ->
         resolve resolve_info src
      in
      match Arg.eval_arglist ctx.variables ~field_name:field.name field.args query_field.arguments resolver with
      | Ok unlifted_value ->
          let lifted_value =
            field.lift unlifted_value
            |> Io.Result.map_error ~f:(fun (err: err) -> `Resolve_error (err, path')) >>=? fun resolved ->
            present { ctx with ctx = !ctx_ref } resolved query_field field.typ path'
          in
          lifted_value >>| (function
          | Ok (value, errors) ->
              Ok ((name, value), errors)
          | Error (`Argument_error _)
          | Error (`Validation_error _) as error ->
              error
          | Error (`Resolve_error err) as error ->
              match field.typ with
              | NonNullable _ ->
                  error
              | _ ->
                 Ok ((name, `Null), [err])
          )
      | Error err ->
          Io.error (`Argument_error err)

  and resolve_fields : type src. execution_context -> ?execution_order:execution_order -> src -> (src) obj -> Graphql_parser.field list -> path -> (json * error list, [> resolve_error]) result Io.t =
    fun ctx ?execution_order:(execution_order=Parallel) src obj fields path ->
      map_fields_with_order execution_order (fun (query_field : Graphql_parser.field) ->
        let name = alias_or_name query_field in
        if query_field.name = "__typename" then
          Io.ok ((name, `String obj.name), [])
        else
          match field_from_object obj query_field.name with
          | Some field ->
              resolve_field ctx src query_field field path
          | None ->
              let err = Printf.sprintf "Field '%s' is not defined on type '%s'" query_field.name obj.name in
              Io.error (`Validation_error err)
      ) fields
      |> Io.map ~f:List.Result.join
      |> Io.Result.map ~f:(fun xs -> (`Assoc (List.map fst xs), List.map snd xs |> List.concat))

  let data_to_json = function
    | data, [] -> `Assoc ["data", data]
    | data, (errors: error list) ->
        let errors = List.map (fun ((err: err), path) -> error_to_json ~path ~extensions:(Err.extensions_of_error err) (Err.message_of_error err)) errors in
        `Assoc [
          "errors", `List errors;
          "data", data;
        ]

  let rec present_fast : type src. execution_context -> src -> Graphql_parser.field -> (src) typ -> path -> (json * error list, [> resolve_error]) result =
    fun ctx src query_field typ path ->
      match typ with
      | Scalar s -> (match src with
                    | None -> Ok (`Null, [])
                    | Some src' -> Ok (s.coerce src', []))
      | List t ->
          (match src with
           | None -> Ok (`Null, [])
           | Some src' ->
              let x = (List.mapi (fun i x -> present_fast ctx x query_field t ((`Int i)::path)) src')
                      |> List.Result.join in
              match x with
              | Error e -> Error e
              | Ok xs -> Ok (`List (List.map fst xs), List.map snd xs |> List.concat)
          )
      | NonNullable t -> present_fast ctx (Some src) query_field t path
      | Object o ->
         (match src with
          | None -> Ok (`Null, [])
          | Some src' ->
            match collect_fields ctx o query_field.selection_set with
            | Ok fields -> resolve_fields_fast ctx src' o fields path
            | Error e -> Error (`Argument_error e))
      | Enum e ->
         (match src with
          | None -> Ok (`Null, [])
          | Some src' ->
            match List.find (fun enum_value -> src' = enum_value.value) e.values with
            | Some enum_value -> Ok (`String enum_value.name, [])
            | None -> Ok (`Null, [])
          )
      | Abstract _ ->
          (match src with
          | None -> Ok (`Null, [])
          | Some (AbstractValue (typ', src')) ->
            present_fast ctx (Some src') query_field typ' path
          )

  and resolve_field_fast : type src. execution_context -> src -> Graphql_parser.field -> (src) fast_field -> path -> ((string * json) * error list, [> resolve_error]) result =
    fun ctx src query_field (FastField field) path ->
      let name = alias_or_name query_field in
      let path' = (`String name)::path in
      let resolve_info = {
        ctx = ctx.ctx;
        field = query_field;
        fragments = ctx.fragments;
        variables = ctx.variables;
        operation = ctx.operation;
      } in
      let ctx_ref = ref ctx.ctx in
      let resolver = match field.resolve with
      | `Resolve_with_set_child_context resolve ->
         let set_child_context = (fun f ->
           let new_ctx = (f !ctx_ref) in
           ctx_ref := new_ctx;
           new_ctx) in
         resolve set_child_context resolve_info src
      | `Resolve resolve ->
         resolve resolve_info src
      in
      match Arg.eval_arglist ctx.variables ~field_name:field.name field.args query_field.arguments resolver with
      | Ok unlifted_value ->
          (* let lifted_value = *)
          (*   field.lift unlifted_value *)
          (*   |> Io.Result.map_error ~f:(fun (err: err) -> `Resolve_error (err, path')) >>=? fun resolved -> *)
          (*   present { ctx with ctx = !ctx_ref } resolved query_field field.typ path' *)
          (* in *)
          (* lifted_value >>| (function *)
          let lifted_value = field.lift unlifted_value in
          let lifted_value = match lifted_value with
          | Error err -> Error (`Resolve_error (err, path'))
          | Ok resolved ->
            present_fast { ctx with ctx = !ctx_ref } resolved query_field field.typ path'
          in
          let res = match lifted_value with
          | Ok (value, errors) ->
              Ok ((name, value), errors)
          | Error (`Argument_error _)
          | Error (`Validation_error _) as error ->
              error
          | Error (`Resolve_error err) as error ->
              match field.typ with
              | NonNullable _ ->
                  error
              | _ ->
                 Ok ((name, `Null), [err]) in
          res
      | Error err ->
          Error (`Argument_error err)

  and resolve_fields_fast : type src. execution_context -> ?execution_order:execution_order -> src -> (src) obj -> Graphql_parser.field list -> path -> (json * error list, [> resolve_error]) result =
    fun ctx ?execution_order:(execution_order=Parallel) src obj fields path ->

    let res = List.map (fun (query_field : Graphql_parser.field) ->
        let name = alias_or_name query_field in
        if query_field.name = "__typename" then
          Ok ((name, `String obj.name), [])
        else
          match fast_field_from_object obj query_field.name with
          | Some field ->
              resolve_field_fast ctx src query_field field path
          | None ->
              let err = Printf.sprintf "Field '%s' is not defined on type '%s'" query_field.name obj.name in
              Error (`Validation_error err)
      ) fields
      |> List.Result.join in
    match res with
    | Error e -> Error e
    | Ok xs -> Ok (`Assoc (List.map fst xs), List.map snd xs |> List.concat)


  let to_response = function
    | Ok _ as res -> res
    | Error `No_operation_found ->
        Error (error_response "No operation found")
    | Error `Operation_not_found ->
        Error (error_response "Operation not found")
    | Error `Operation_name_required ->
        Error (error_response "Operation name required")
    | Error `Subscriptions_not_configured ->
        Error (error_response "Subscriptions not configured")
    | Error `Mutations_not_configured ->
        Error (error_response "Mutations not configured")
    | Error (`Validation_error msg) ->
        Error (error_response msg)
    | Error (`Argument_error msg) ->
        let `Assoc errors = error_response msg in
        Error (`Assoc (("data", `Null)::errors))
    | Error (`Resolve_error (err, path)) ->
       let `Assoc errors = error_response ~path ~extensions:(Err.extensions_of_error err) (Err.message_of_error err) in
        Error (`Assoc (("data", `Null)::errors))

  let subscribe : execution_context -> subscription_field -> Graphql_parser.field -> (json response Io.Stream.t, [> resolve_error]) result Io.t
  =
    fun ctx (SubscriptionField subs_field) field ->
      let open Io.Infix in
      let name = alias_or_name field in
      let path = [`String name] in
      let resolve_info = {
        ctx = ctx.ctx;
        field;
        fragments = ctx.fragments;
        variables = ctx.variables;
        operation = ctx.operation
      } in
      let resolver = subs_field.resolve resolve_info in
      match Arg.eval_arglist ctx.variables ~field_name:subs_field.name subs_field.args field.arguments resolver with
      | Ok result ->
          result
          |> Io.Result.map ~f:(fun source_stream ->
            Io.Stream.map source_stream (fun value ->
              present ctx value field subs_field.typ path
              |> Io.Result.map ~f:(fun (data, errors) ->
                data_to_json (`Assoc [name, data], errors)
              )
              >>| to_response
            )
          )
          |> Io.Result.map_error ~f:(fun (err: err) -> `Resolve_error (err, path))
      | Error err -> Io.error (`Argument_error err)

  let execute_operation : schema -> execution_context -> Graphql_parser.operation -> ([ `Response of json | `Stream of json response Io.Stream.t], [> execute_error]) result Io.t =
    fun schema ctx operation ->
      let open Io.Infix in
      match operation.optype with
      | Graphql_parser.Query ->
          let query  = schema.query in
          Io.return (collect_fields ctx query operation.selection_set)
          |> Io.Result.map_error ~f:(fun e -> `Argument_error e) >>=? fun fields ->
          (* (resolve_fields ctx () query fields [] : (json * error list, resolve_error) result Io.t :> (json * error list, [> execute_error]) result Io.t) *)
          (Io.return (resolve_fields_fast ctx () query fields []) : (json * error list, resolve_error) result Io.t :> (json * error list, [> execute_error]) result Io.t)
          |> Io.Result.map ~f:(fun data_errs -> `Response (data_to_json data_errs))
      | Graphql_parser.Mutation ->
          begin match schema.mutation with
          | None -> Io.error `Mutations_not_configured
          | Some mut ->
              Io.return (collect_fields ctx mut operation.selection_set)
              |> Io.Result.map_error ~f:(fun e -> `Argument_error e) >>=? fun fields ->
              (resolve_fields ~execution_order:Serial ctx () mut fields [] : (json * error list, resolve_error) result Io.t :> (json * error list, [> execute_error]) result Io.t)
              |> Io.Result.map ~f:(fun data_errs -> `Response (data_to_json data_errs))
          end
      | Graphql_parser.Subscription ->
          begin match schema.subscription with
          | None -> Io.error `Subscriptions_not_configured
          | Some subs ->
              Io.return (collect_fields ctx (obj_of_subscription_obj subs) operation.selection_set)
              |> Io.Result.map_error ~f:(fun e -> `Argument_error e) >>=? fun fields ->
              begin match fields with
              | [field] ->
                  (match field_from_subscription_object subs field.name with
                   | Some subscription_field ->
                       (subscribe ctx subscription_field field : ((json, json) result Io.Stream.t, resolve_error) result Io.t :> ((json, json) result Io.Stream.t, [> execute_error]) result Io.t)
                       |> Io.Result.map ~f:(fun stream -> `Stream stream)
                   | None -> Io.ok (`Response (`Assoc [(alias_or_name field, `Null)])))
              (* see http://facebook.github.io/graphql/June2018/#sec-Response-root-field *)
              | _ -> Io.error (`Validation_error "Subscriptions only allow exactly one selection for the operation.")
              end
          end

  let collect_fragments doc =
    List.fold_left (fun memo -> function
      | Graphql_parser.Operation _ -> memo
      | Graphql_parser.Fragment f -> StringMap.add f.name f memo
    ) StringMap.empty doc

  exception FragmentCycle of string list
  let rec validate_fragments fragment_map =
    try
      StringMap.iter (fun name _ ->
        validate_fragment fragment_map StringSet.empty name
      ) fragment_map;
      Ok fragment_map
    with FragmentCycle fragment_names ->
      let cycle = String.concat ", " fragment_names in
      let err = Format.sprintf "Fragment cycle detected: %s" cycle in
      Error (`Validation_error err)

  and validate_fragment (fragment_map : fragment_map) visited name =
    match StringMap.find name fragment_map with
    | None -> ()
    | Some fragment when StringSet.mem fragment.name visited ->
        raise (FragmentCycle (StringSet.elements visited))
    | Some fragment ->
        let visited' = StringSet.add fragment.name visited in
        List.iter (validate_fragment_selection fragment_map visited') fragment.selection_set

  and validate_fragment_selection fragment_map visited selection =
    match selection with
    | Graphql_parser.Field field ->
        List.iter (validate_fragment_selection fragment_map visited) field.selection_set
    | InlineFragment inline_fragment ->
        List.iter (validate_fragment_selection fragment_map visited) inline_fragment.selection_set
    | FragmentSpread fragment_spread ->
        validate_fragment fragment_map visited fragment_spread.name

  let collect_and_validate_fragments doc =
    let fragments = collect_fragments doc in
    validate_fragments fragments

  let collect_operations doc =
    List.fold_left (fun memo -> function
      | Graphql_parser.Operation op -> op::memo
      | Graphql_parser.Fragment _ -> memo
    ) [] doc

  let select_operation ?operation_name doc =
    let operations = collect_operations doc in
    match operation_name, operations with
    | _, [] -> Error `No_operation_found
    | None, [op] -> Ok op
    | None, _::_ -> Error `Operation_name_required
    | Some name, ops ->
        try
          Ok (List.find_exn (fun op -> op.Graphql_parser.name = Some name) ops)
        with Not_found ->
          Error `Operation_not_found
  let prepare_schema schema =
    Introspection.add_schema_field schema

  let execute schema ctx ?variables:(variables=[]) ?operation_name doc =
    let open Io.Infix in
    let execute' schema ctx doc =
      Io.return (collect_and_validate_fragments doc) >>=? fun fragments ->
      let variables = List.fold_left (fun memo (name, value) -> StringMap.add name value memo) StringMap.empty variables in

      (* let schema' = Introspection.add_schema_field schema in *)
      Io.return (select_operation ?operation_name doc) >>=? fun op ->
        let execution_ctx = { fragments; ctx; variables; operation = op } in
        execute_operation schema (* schema' *) execution_ctx op
    in
    execute' schema ctx doc >>| to_response
end
