open Dream_middleware_ext.Rate_limiter.FixedWindow
(* open Dream_middleware_ext.Rate_limiter.TokenBucket *)

(* make fixed window strategy conf*)
let fixed_window_conf = make_fw_conf ~limit:5L ~secs:5. ~return_headers:true

(* make global store that tracks values *)
let fixed_window_store = make_fw_store fixed_window_conf

(* make fixed windows rate limiter middleware *)
let fixed_window_middleware =
  make_fw_rate_limiter ~conf:fixed_window_conf ~store:fixed_window_store

(* or *)

(* make token bucket strategy conf*)
(* let token_bucket_conf =
  make_tb_conf ~bucket_size:5L ~refill_count:6L ~refill_interval:5. *)

(* make global bucket that tracks values *)
(* let bucket = make_tb_bucket token_bucket_conf *)

(* make token bucket rate limiter middleware *)
(* let token_bucket_middleware =
  make_tb_rate_limiter ~conf:token_bucket_conf ~bucket
    ~err_msg:"Request limit exceeded, the police will be notified" *)

let () =
  Dream.run ~port:9090
  @@ fixed_window_middleware (* apply fixed window or token bucket middleware *)
  @@ Dream.router
       [
         Dream.scope "/" []
           [ Dream.get "/rate_limit" (fun _ -> Dream.respond "") ];
       ]
