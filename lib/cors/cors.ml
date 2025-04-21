type cors_conf = {
  allowed_origin : allowed_origin_type;
  allowed_methods : string list;
  allowed_headers : string list;
  expose_headers : string list;
  max_age : float;
  preflight : bool;
  set_vary_header : bool;
}

and allowed_origin_type =
  | WildCard
  | OriginUrl of (credential_perm * string)
  | OriginUrlFn of (credential_perm * string_fn)

and credential_perm = Allow | Disallow
and string_fn = unit -> string

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
  | HeadersNotAllowed

let all_verbs_header () =
  [
    "GET";
    "POST";
    "PUT";
    "DELETE";
    "HEAD";
    "OPTIONS";
    "CONNECT";
    "PATCH";
    "TRACE";
  ]

let is_valid_origin o =
  Uri.of_string o |> Uri.Absolute_http.of_uri |> Result.is_ok
  && (not @@ String.contains o '*')

let is_valid_origin_ty o =
  match o with
  | OriginUrl (_, url) -> if is_valid_origin url then true else false
  | OriginUrlFn (_, url_fn) ->
      if is_valid_origin @@ url_fn () then true else false
  | _ -> true

let is_valid_method m =
  match Dream.string_to_method m with `Method _ -> false | _ -> true

let is_wildcard s = String.contains s '*'

let is_valid_methods ao am =
  match ao with
  | WildCard ->
      if List.for_all (fun elt -> is_valid_method elt) am then true else false
  | OriginUrl (ac, _) | OriginUrlFn (ac, _) -> (
      match ac with
      | Allow ->
          if List.length am == 1 && List.hd am == "*" then true
          else if List.for_all (fun elt -> is_valid_method elt) am then true
          else false
      | Disallow ->
          if List.for_all (fun elt -> is_valid_method elt) am then true
          else false)

let is_valid_headers ao ah =
  let open Utils in
  match ao with
  | WildCard ->
      if
        List.for_all
          (fun elt -> (not @@ is_wildcard elt) && (not @@ is_str_empty elt))
          ah
      then true
      else false
  | OriginUrl (ac, _) | OriginUrlFn (ac, _) -> (
      match ac with
      | Allow ->
          if List.length ah == 1 && List.hd ah == "*" then true
          else if
            List.for_all
              (fun elt -> (not @@ is_wildcard elt) && (not @@ is_str_empty elt))
              ah
          then true
          else false
      | Disallow ->
          if
            List.for_all
              (fun elt -> (not @@ is_wildcard elt) && (not @@ is_str_empty elt))
              ah
          then true
          else false)

let is_valid_max_age ma = if ma < 0. || ma > 86400. then false else true

let make_cors_conf ~allowed_origin ~allowed_methods ~allowed_headers
    ~expose_headers ?(max_age = 7200.) ?(preflight = true)
    ?(set_vary_header = true) () =
  if not @@ is_valid_max_age max_age then
    raise (InvalidMaxAge "max_age: Max age must be between 0 and 86400 seconds")
  else if not @@ is_valid_origin_ty allowed_origin then
    raise (InvalidOrigin "allowed_origin: Origin url malformed")
  else if not @@ is_valid_methods allowed_origin allowed_methods then
    raise (InvalidMethod "allowed_methods: Method(s) not valid")
  else if not @@ is_valid_headers allowed_origin allowed_headers then
    raise (InvalidAllowedHeaders "allowed_headers: Allowed header(s) not valid")
  else if not @@ is_valid_headers allowed_origin expose_headers then
    raise (InvalidExposeHeaders "expose_headers: Expose header(s) not valid")
  else
    {
      allowed_origin;
      allowed_methods;
      allowed_headers;
      expose_headers;
      max_age;
      preflight;
      set_vary_header;
    }

let hdr_list_of_str hdr_str = String.split_on_char ',' hdr_str

let hdr_str_of_list hdr_lst =
  let rec intersperse sep lst =
    match lst with
    | [] | [ _ ] -> lst
    | hd :: tl -> hd :: sep :: intersperse sep tl
  in
  List.fold_left ( ^ ) "" @@ intersperse "," hdr_lst

let is_preflight req =
  if Dream.method_ req == `OPTIONS && Dream.has_header req "Origin" then true
  else false

let validate_origin conf req =
  match Dream.header req "Origin" with
  | Some "null" -> (
    match conf.allowed_origin with
    | WildCard -> Ok true
    | _ -> Error OriginNotAllowed
  )
  | Some orig -> (
      if not @@ is_valid_origin orig then Error OriginNotAllowed
      else
        match conf.allowed_origin with
        | OriginUrl (_, url) ->
            if orig = url then Ok true else Error OriginNotAllowed
        | OriginUrlFn (_, url_fn) ->
            if orig = url_fn () then Ok true else Error OriginNotAllowed
        | _ -> Ok true)
  | None -> Error MissingOrigin

let validate_request_method conf req =
  match Dream.header req "Access-Control-Request-Method" with
  | Some m ->
      let ms = conf.allowed_methods in
      if List.mem m ms && m != "*" then Ok true else Error MethodsNotAllowed
  | None -> Error MissingMethods

let validate_request_headers conf req =
  match Dream.header req "Access-Control-Request-Headers" with
  | Some h ->
      let hrds = conf.allowed_headers in
      if List.for_all (fun elt -> List.mem elt hrds) (hdr_list_of_str h) then
        Ok true
      else Error HeadersNotAllowed
  | None -> Error MissingHeaders

let cors_err_resp = function
  | MissingOrigin -> Dream.respond ~status:`Bad_Request "Origin is missing"
  | OriginNotAllowed ->
      Dream.respond ~status:`Bad_Request "Origin is not allowed to make request"
  | MissingMethods ->
      Dream.respond ~status:`Bad_Request
        "`Access-Control-Request-Method` header is required but not provided"
  | MethodsNotAllowed ->
      Dream.respond ~status:`Bad_Request "Requested method(s) not allowed"
  | MissingHeaders ->
      Dream.respond ~status:`Bad_Request
        "`Access-Control-Request-Headers` header is required but not provided"
  | HeadersNotAllowed ->
      Dream.respond ~status:`Bad_Request "Request header(s) not allowed"

let set_allowed_origin_pf conf hrd =
  match conf.allowed_origin with
  | WildCard -> [ ("Access-Control-Allow-Origin", "*") ] @ hrd
  | OriginUrl (ac, url) ->
      if ac == Allow then
        [ ("Access-Control-Allow-Credentials", "true") ]
        @ [ ("Access-Control-Allow-Origin", url) ]
        @ hrd
      else [ ("Access-Control-Allow-Origin", url) ] @ hrd
  | OriginUrlFn (ac, url_fn) ->
      if ac == Allow then
        [ ("Access-Control-Allow-Origin", url_fn ()) ]
        @ [ ("Access-Control-Allow-Credentials", "true") ]
        @ hrd
      else [ ("Access-Control-Allow-Origin", url_fn ()) ] @ hrd

let build_allowed_origin conf rep =
  match conf.allowed_origin with
  | WildCard -> Dream.add_header rep "Access-Control-Allow-Origin" "*"
  | OriginUrl (ac, url) ->
      if ac == Allow then (
        Dream.add_header rep "Access-Control-Allow-Credentials" "true";
        Dream.add_header rep "Access-Control-Allow-Origin" url)
      else Dream.add_header rep "Access-Control-Allow-Origin" url
  | OriginUrlFn (ac, url_fn) ->
      if ac == Allow then (
        Dream.add_header rep "Access-Control-Allow-Origin" @@ url_fn ();
        Dream.add_header rep "Access-Control-Allow-Credentials" "true")
      else Dream.add_header rep "Access-Control-Allow-Origin" @@ url_fn ()

let set_req_headers_pf conf hrd =
  let lst = conf.allowed_headers in
  let hdrs = hdr_str_of_list lst in
  [ ("Access-Control-Allow-Headers", hdrs) ] @ hrd

let set_req_methods_pf conf hrd =
  let lst = conf.allowed_methods in
  let mtds = hdr_str_of_list lst in
  [ ("Access-Control-Allow-Methods", mtds) ] @ hrd

let set_vary_header_pf req hrd =
  match Dream.header req "Vary" with
  | Some h ->
      [
        ( "Vary",
          h
          ^ ", Origin, Access-Control-Request-Method, \
             Access-Control-Request-Headers" );
      ]
      @ hrd
  | None ->
      [
        ( "Vary",
          "Origin, Access-Control-Request-Method, \
           Access-Control-Request-Headers" );
      ]
      @ hrd

let build_vary_header req =
  match Dream.header req "Vary" with
  | Some h ->
      Dream.add_header req "Vary"
      @@ h
      ^ ", Origin, Access-Control-Request-Method, \
         Access-Control-Request-Headers"
  | None ->
      Dream.add_header req "Vary"
        "Origin, Access-Control-Request-Method, Access-Control-Request-Headers"

let set_max_age_pf conf hrd =
  [ ("Access-Control-Max-Age", string_of_int @@ int_of_float conf.max_age) ]
  @ hrd

let non_preflight_headers req =
  let ao = Dream.header req "Access-Control-Allow-Origin" |> Option.get in
  let eh = Dream.header req "Access-Control-Expose-Headers" |> Option.get in
  let vh = Dream.header req "Vary" |> Option.get in
  [ ("Access-Control-Allow-Origin", ao) ]
  @ [ ("Access-Control-Expose-Headers", eh) ]
  @ [ ("Vary", vh) ]

let handle_cors is_preflight conf inner_handler req =
  let hrd = [] in
  let vo = validate_origin conf req in
  let vrm = validate_request_method conf req in
  let vrh = validate_request_headers conf req in
  if Result.is_error vo then cors_err_resp @@ Result.get_error vo
  else if is_preflight then
    if Result.is_error vrm then cors_err_resp @@ Result.get_error vrm
    else if Result.is_error vrh then cors_err_resp @@ Result.get_error vrh
    else
      let h_ao = set_allowed_origin_pf conf hrd in
      let h_ma = set_max_age_pf conf hrd in
      let h_rh = set_req_headers_pf conf hrd in
      let h_rm = set_req_methods_pf conf hrd in
      let h_n = h_ao @ h_ma @ h_rh @ h_rm in
      if conf.set_vary_header then
        let h_v = set_vary_header_pf req hrd in
        let hrd = h_n @ h_v in
        Dream.respond ~headers:hrd ~status:`No_Content ""
      else Dream.respond ~headers:h_n ~status:`No_Content ""
  else (
    let rep : Dream.response Lwt.t = inner_handler req in
    let%lwt rep = rep in
    build_allowed_origin conf rep;
    Dream.add_header rep "Access-Control-Expose-Headers"
    @@ hdr_str_of_list conf.expose_headers;
    if conf.set_vary_header then build_vary_header rep;
    print_all_headers rep; (* Debug *)
    Lwt.return rep)

let make_cors : cors_conf -> Dream.middleware = 
  fun conf ->
  fun inner_handler req ->
    let is_preflight = conf.preflight && is_preflight req in
    handle_cors is_preflight conf inner_handler req
