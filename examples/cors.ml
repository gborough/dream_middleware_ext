open Dream_middleware_ext.Cors

(* allow all verb headers *)
let all_verb_headers = all_verbs_header ()

(* make conf: allow wildcard origin *)
let allow_wildcard_conf =
  make_cors_conf ~allowed_origin:WildCard ~allowed_methods:all_verb_headers
    ~allowed_headers:[ "X-Requested-With" ] ~expose_headers:[ "GET" ] ()

(* or *)

(* make conf: single origin that allows credential*)
let _allow_cred_conf =
  make_cors_conf
    ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
    ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "X-Requested-With" ]
    ~expose_headers:[ "GET" ] ()

(* or *)

(* make conf: single origin that disallows credential*)
let _disallow_cred_conf =
  make_cors_conf
    ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
    ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "X-Requested-With" ]
    ~expose_headers:[ "GET" ] ()

(* make allow wildcard middle or with other confs *)
let cors_middleware = make_cors allow_wildcard_conf

let preflight_route =
  Dream.post "/preflight" (fun _ -> Dream.respond ~status:`OK "OK")

(* return CORS relevant headers in non_preflight requests *)
let _non_preflight_route =
  Dream.post "/non_preflight" (fun req ->
      (* block start *)
      let non_preflight_headers = non_preflight_headers req in
      List.iter
        (fun (hrd, v) -> Dream.add_header req hrd v)
        non_preflight_headers;
      (* block end *)
      Dream.respond ~status:`OK "")

let () =
  Dream.run ~port:8080
  @@ cors_middleware (* should apply CORS middileware at outmost level*)
  @@ Dream.router
       [ Dream.scope "/" [] [ preflight_route (* non_preflight_route *) ] ]
