open Dream_middleware_ext.Traffic_filter

(* make ip based conf that allows all traffic *)
let ip_based_all_traffic_conf =
  make_traffic_filter_conf
    ~filter_type:
      (Ips
         {
           whitelist = [ "127.0.0.1" ];
           blacklist = [ "127.0.0.2" ];
           x_real_ip = true;
         })
    ~target:RootAccess ()

(* or *)

(* make ip based conf that allows traffic on specific paths *)
(* let _ip_based_path_list_conf =
  make_traffic_filter_conf
    ~filter_type:
      (Ips
         {
           whitelist = [ "127.0.0.1" ];
           blacklist = [ "127.0.0.2" ];
           x_real_ip = true;
         })
    ~target:(PathList [ "allow_here"; "allow_there" ])
    () *)

(* or *)

(* make header based conf that allows traffic on specific paths *)
(* let _header_based_conf =
  make_traffic_filter_conf
    ~filter_type:(Headers [ "THIS_HEADER"; "THAT_HEADER" ])
    ~target:RootAccess () *)

(* or *)

(* make cookie based conf that allows traffic on specific paths *)
(* let _cookie_based_conf =
  make_traffic_filter_conf
    ~filter_type:(Cookies [ "THIS_COOKIE"; "THAT_COOKIE" ])
    ~target:RootAccess () *)

(* make ip based middleware or with other confs *)
let ip_filter_middleware =
  make_traffic_filter ~conf:ip_based_all_traffic_conf
    ~err_msg:"Unauthorised access, the police will be notified"

let () =
  Dream.run ~port:9090
  @@ ip_filter_middleware (* apply ip_filter or other middlewares *)
  @@ Dream.router
       [
         Dream.scope "/" []
           [ Dream.get "/rate_limit" (fun _ -> Dream.respond "") ];
       ]
