(** Cross-Origin Resource Sharing(CORS) middleware *)

type cors_conf
(** The type of configuration for CORS *)

type allowed_origin_type =
  | WildCard
  | OriginUrl of (credential_perm * string)
  | OriginUrlFn of (credential_perm * string_fn)
      (** The type of Access-Control-Allow-Origin and
          Access-Control-Allow-Credentials combinations: 1. WildCard -> i.e.
          "*", which allows all origins and permits no credentials 2. OriginUrl
          -> single origin with credential permission 3. OriginUrlFn -> a
          function type that returns a single origin with credential permission
      *)

and credential_perm =
  | Allow
  | Disallow
      (** The type for allowing or disallowing Access-Control-Allow-Credentials
      *)

and string_fn = unit -> string
(** Function type that returns a single origin, i.e. matching on multiple
    origins *)

exception InvalidOrigin of string
exception InvalidMethod of string
exception InvalidMaxAge of string
exception InvalidAllowedHeaders of string
exception InvalidExposeHeaders of string

type cors_error =
  | OriginNotAllowed
  | MissingOrigin
  | MissingMethods
  | MissingHeaders
  | MethodsNotAllowed
  | HeadersNotAllowed  (** The type of CORS errors *)

val all_verbs_header : unit -> string list
(** Helper function to make a header list of all http verbs. *)

val non_preflight_headers : Dream.request -> (string * string) list
(** Helper function to return CORS relevant headers in non-preflight response.
*)

val make_cors_conf :
  allowed_origin:allowed_origin_type ->
  allowed_methods:string list ->
  allowed_headers:string list ->
  expose_headers:string list ->
  ?max_age:float ->
  ?preflight:bool ->
  ?set_vary_header:bool ->
  unit ->
  cors_conf
(** [make_cors_conf args...] makes a CORS configuration. *)

val make_cors :
  cors_conf -> Dream.handler -> Dream.request -> Dream.response Lwt.t
(** [make_cors cors_conf] takes a CORS configuration and makes a CORS middleware
*)
