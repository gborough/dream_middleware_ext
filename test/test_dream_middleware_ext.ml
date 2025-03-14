open Dream_middleware_ext

(* overlapping ips *)
let _ =
  let open Traffic_filter in
  try
    let _ =
      make_traffic_filter_conf
        ~filter_type:
          (Ips
             {
               whitelist = [ "127.0.0.1" ];
               blacklist = [ "127.0.0.1" ];
               x_real_ip = false;
             })
        ~target:(PathList [ "/a" ]) ()
    in
    Printf.printf "%s" ""
  with
  | OverlappingIps _ -> ()
  | _ -> exit 1

(* invalid ips *)
let _ =
  let open Traffic_filter in
  try
    let _ =
      make_traffic_filter_conf
        ~filter_type:
          (Ips
             {
               whitelist = [ "127.0.0.b" ];
               blacklist = [ "127.0.0.1" ];
               x_real_ip = false;
             })
        ~target:(PathList [ "/client" ]) ()
    in
    Printf.printf "%s" ""
  with
  | InvalidIp _ -> ()
  | _ -> exit 1

(* invalid path list containing root path *)
let _ =
  let open Traffic_filter in
  try
    let _ =
      make_traffic_filter_conf
        ~filter_type:
          (Ips
             {
               whitelist = [ "127.0.0.1" ];
               blacklist = [ "127.0.0.2" ];
               x_real_ip = false;
             })
        ~target:(PathList [ "/"; "/a" ])
        ()
    in
    Printf.printf "%s" ""
  with
  | InvalidTarget _ -> ()
  | _ -> exit 1

(* invalid path containing empty *)
let _ =
  let open Traffic_filter in
  try
    let _ =
      make_traffic_filter_conf
        ~filter_type:
          (Ips
             {
               whitelist = [ "127.0.0.1" ];
               blacklist = [ "127.0.0.2" ];
               x_real_ip = false;
             })
        ~target:(PathList [ "" ]) ()
    in
    Printf.printf "%s" ""
  with
  | InvalidTarget _ -> ()
  | _ -> exit 1

(* headers empty*)
let _ =
  let open Traffic_filter in
  try
    let _ =
      make_traffic_filter_conf ~filter_type:(Headers [ "" ])
        ~target:(PathList [ "/a" ]) ()
    in
    Printf.printf "%s" ""
  with
  | EmptyHeaderList _ -> ()
  | _ -> exit 1

(* cookie empty*)
let _ =
  let open Traffic_filter in
  try
    let _ =
      make_traffic_filter_conf ~filter_type:(Cookies [ "" ])
        ~target:(PathList [ "/a" ]) ()
    in
    Printf.printf "%s" ""
  with
  | EmptyCookieList _ -> ()
  | _ -> exit 1

let%expect_test "allowed_ip" =
  let open Traffic_filter in
  let traffic_filter_conf =
    make_traffic_filter_conf
      ~filter_type:
        (Ips
           {
             whitelist = [ "127.0.0.1" ];
             blacklist = [ "127.0.0.2" ];
             x_real_ip = false;
           })
      ~target:(PathList [ "/allowed" ]) ()
  in
  let middleware = make_traffic_filter ~conf:traffic_filter_conf in
  let pipeline = Dream.pipeline [ middleware ] in

  Test_utils.show "/allowed"
  @@ Dream.router
       [
         Dream.scope "/" [ pipeline ]
           [ Dream.get "/allowed" (fun _ -> Dream.respond "") ];
       ];
  [%expect {| Response: 200 OK |}]

let%expect_test "disallowed_ip" =
  let open Traffic_filter in
  let traffic_filter_conf =
    make_traffic_filter_conf
      ~filter_type:
        (Ips
           {
             whitelist = [ "127.0.0.2" ];
             blacklist = [ "127.0.0.1" ];
             x_real_ip = false;
           })
      ~target:(PathList [ "/disallowed" ]) ()
  in
  let middleware = make_traffic_filter ~conf:traffic_filter_conf in
  let pipeline = Dream.pipeline [ middleware ] in

  Test_utils.show "/disallowed"
  @@ Dream.router
       [
         Dream.scope "/" [ pipeline ]
           [ Dream.get "/disallowed" (fun _ -> Dream.respond "") ];
       ];
  [%expect {| Response: 401 Unauthorized |}]

let%expect_test "allowed_header" =
  let open Traffic_filter in
  let traffic_filter_conf =
    make_traffic_filter_conf
      ~filter_type:(Headers [ "A"; "B" ])
      ~target:(PathList [ "/allowed_header" ]) ()
  in
  let middleware = make_traffic_filter ~conf:traffic_filter_conf in
  let pipeline = Dream.pipeline [ middleware ] in

  Test_utils.show ~headers:[ ("A", "A"); ("B", "B") ] "/allowed_header"
  @@ Dream.router
       [
         Dream.scope "/" [ pipeline ]
           [ Dream.get "/allowed_header" (fun _ -> Dream.respond "") ];
       ];
  [%expect {| Response: 200 OK |}]

let%expect_test "disallowed_header" =
  let open Traffic_filter in
  let traffic_filter_conf =
    make_traffic_filter_conf
      ~filter_type:(Headers [ "C"; "D" ])
      ~target:(PathList [ "/disallowed_header" ]) ()
  in
  let middleware = make_traffic_filter ~conf:traffic_filter_conf in
  let pipeline = Dream.pipeline [ middleware ] in

  Test_utils.show ~headers:[ ("A", "A"); ("B", "B") ] "/disallowed_header"
  @@ Dream.router
       [
         Dream.scope "/" [ pipeline ]
           [ Dream.get "/disallowed_header" (fun _ -> Dream.respond "") ];
       ];
  [%expect {| Response: 401 Unauthorized |}]

(* invalid limit *)
let _ =
  let open Rate_limiter.FixedWindow in
  try
    let _ = make_fw_conf ~limit:0L ~secs:1. ~return_headers:false in
    Printf.printf "%s" ""
  with
  | InvalidLimit _ -> ()
  | _ -> exit 1

(* invalid time window*)
let _ =
  let open Rate_limiter.FixedWindow in
  try
    let _ = make_fw_conf ~limit:1L ~secs:0. ~return_headers:false in
    Printf.printf "%s" ""
  with
  | InvalidTimeWindow _ -> ()
  | _ -> exit 1

(* invalid bucket size *)
let _ =
  let open Rate_limiter.TokenBucket in
  try
    let _ = make_tb_conf ~bucket_size:0L ~refill_count:1L ~refill_interval:1. in
    Printf.printf "%s" ""
  with
  | InvalidBucketSize _ -> ()
  | _ -> exit 1

(* invalid refill rate *)
let _ =
  let open Rate_limiter.TokenBucket in
  try
    let _ = make_tb_conf ~bucket_size:1L ~refill_count:1L ~refill_interval:0. in
    Printf.printf "%s" ""
  with
  | InvalidRefillRate _ -> ()
  | _ -> exit 1

(* invalid refill count*)
let _ =
  let open Rate_limiter.TokenBucket in
  try
    let _ = make_tb_conf ~bucket_size:1L ~refill_count:0L ~refill_interval:1. in
    Printf.printf "%s" ""
  with
  | InvalidRefillCount _ -> ()
  | _ -> exit 1

(* invalid refill count *)
let _ =
  let open Rate_limiter.TokenBucket in
  try
    let _ = make_tb_conf ~bucket_size:1L ~refill_count:2L ~refill_interval:1. in
    Printf.printf "%s" ""
  with
  | InvalidRefillCount _ -> ()
  | _ -> exit 1

(* invalid_origin *)
let _ =
  let open Cors in
  try
    let _ =
      make_cors_conf
        ~allowed_origin:(OriginUrl (Allow, "pardon?"))
        ~allowed_methods:[ "GET"; "POST" ]
        ~allowed_headers:[ "hello"; "X-Requested-With" ]
        ~expose_headers:[ "GET" ] ()
    in
    Printf.printf "%s" ""
  with
  | InvalidOrigin _ -> ()
  | _ -> exit 1

(* invalid method *)
let _ =
  let open Cors in
  try
    let _ =
      make_cors_conf
        ~allowed_origin:(OriginUrl (Allow, "http://localhost"))
        ~allowed_methods:[ "GET"; "BOST" ]
        ~allowed_headers:[ "hello"; "X-Requested-With" ]
        ~expose_headers:[ "GET" ] ()
    in
    Printf.printf "%s" ""
  with
  | InvalidMethod _ -> ()
  | _ -> exit 1

(* invalid allowed header*)
let _ =
  let open Cors in
  try
    let _ =
      make_cors_conf
        ~allowed_origin:(OriginUrl (Disallow, "http://localhost"))
        ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "*" ]
        ~expose_headers:[ "GET" ] ()
    in
    Printf.printf "%s" ""
  with
  | InvalidAllowedHeaders _ -> ()
  | _ -> exit 1

(* invalid empty header *)
let _ =
  let open Cors in
  try
    let _ =
      make_cors_conf
        ~allowed_origin:(OriginUrl (Disallow, "http://localhost"))
        ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "" ]
        ~expose_headers:[ "GET" ] ()
    in
    Printf.printf "%s" ""
  with
  | InvalidAllowedHeaders _ -> ()
  | _ -> exit 1

let%expect_test "preflight_ok" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "A" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`OPTIONS
    ~headers:
      [
        ("Origin", "http://127.0.0.1");
        ("Access-Control-Request-Method", "POST");
        ("Access-Control-Request-Headers", "A");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.options "/a" (fun _ -> Dream.respond "") ] ];
  [%expect
    {|
    Response: 204 No Content
    Access-Control-Allow-Credentials: true
    Access-Control-Allow-Origin: http://127.0.0.1
    Access-Control-Max-Age: 7200
    Access-Control-Allow-Headers: A
    Access-Control-Allow-Methods: GET,POST
    Vary: Origin, Access-Control-Request-Method, Access-Control-Request-Headers |}]

let%expect_test "preflight_invalid_origin" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.2"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "A" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`OPTIONS
    ~headers:
      [
        ("Origin", "http://127.0.0.1");
        ("Access-Control-Request-Method", "POST");
        ("Access-Control-Request-Headers", "A");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.options "/a" (fun _ -> Dream.respond "") ] ];
  [%expect
    {|
    Response: 400 Bad Request
    Origin is not allowed to make request |}]

let%expect_test "preflight_missing_origin" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "A" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`OPTIONS
    ~headers:
      [
        ("Access-Control-Request-Method", "POST");
        ("Access-Control-Request-Headers", "A");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.options "/a" (fun _ -> Dream.respond "") ] ];
  [%expect {|
    Response: 400 Bad Request
    Origin is missing |}]

let%expect_test "preflight_invalid_methods" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "A" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`OPTIONS
    ~headers:
      [
        ("Origin", "http://127.0.0.1");
        ("Access-Control-Request-Method", "DELETE");
        ("Access-Control-Request-Headers", "A");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.options "/a" (fun _ -> Dream.respond "") ] ];
  [%expect
    {|
    Response: 400 Bad Request
    Requested method(s) not allowed |}]

let%expect_test "preflight_missing_methods" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "A" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`OPTIONS
    ~headers:
      [
        ("Origin", "http://127.0.0.1");
        ("Access-Control-Request-Headers", "A");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.options "/a" (fun _ -> Dream.respond "") ] ];
  [%expect
    {|
    Response: 400 Bad Request
    `Access-Control-Request-Method` header is required but not provided |}]

let%expect_test "preflight_invalid_headers" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "B" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`OPTIONS
    ~headers:
      [
        ("Origin", "http://127.0.0.1");
        ("Access-Control-Request-Method", "POST");
        ("Access-Control-Request-Headers", "A");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.options "/a" (fun _ -> Dream.respond "") ] ];
  [%expect
    {|
    Response: 400 Bad Request
    Request header(s) not allowed |}]

let%expect_test "preflight_missing_headers" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "B" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`OPTIONS
    ~headers:
      [
        ("Origin", "http://127.0.0.1");
        ("Access-Control-Request-Method", "POST");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.options "/a" (fun _ -> Dream.respond "") ] ];
  [%expect
    {|
    Response: 400 Bad Request
    `Access-Control-Request-Headers` header is required but not provided |}]

let%expect_test "non_preflight_ok" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "A" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors_non_preflight ~method_:`POST
    ~headers:
      [
        ("Origin", "http://127.0.0.1");
        ("Content-Type", "text/plain");
        ("A", "A");
      ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.post "/a" (fun _ -> Dream.respond "") ] ];
  [%expect
    {|
    Response: 200 OK
    Access-Control-Allow-Origin: http://127.0.0.1
    Access-Control-Expose-Headers: GET
    Vary: Origin, Access-Control-Request-Method, Access-Control-Request-Headers |}]

let%expect_test "non_preflight_missing_origin" =
  let open Cors in
  let cors_conf =
    make_cors_conf
      ~allowed_origin:(OriginUrl (Allow, "http://127.0.0.1"))
      ~allowed_methods:[ "GET"; "POST" ] ~allowed_headers:[ "A" ]
      ~expose_headers:[ "GET" ] ()
  in
  let middleware = make_cors cors_conf in

  Test_utils.show_cors ~method_:`POST
    ~headers:[ ("Content-Type", "text/plain"); ("A", "A") ]
    "/a"
  @@ middleware
  @@ Dream.router
       [ Dream.scope "/" [] [ Dream.post "/a" (fun _ -> Dream.respond "") ] ];
  [%expect {|
    Response: 400 Bad Request
    Origin is missing |}]
