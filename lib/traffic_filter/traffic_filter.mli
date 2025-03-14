(** Traffic filter middleware *)

type traffic_filter_conf
(** The type of configuration for traffic filter *)

type filter_type =
  | Ips of ip_filter
  | Headers of string list
  | Cookies of string list
      (** The type of traffic filter: 1. Ips -> ip based filter , 2. Headers ->
          header based filter 3. Cookies -> cookie based filter *)

and ip_filter = {
  whitelist : string list;
  blacklist : string list;
  x_real_ip : bool;
}
(** The type of ip based filter that takes a whitelist and a blacklist,
    conditionally matching on X-REAL-IP header *)

type target_type =
  | RootAccess
  | PathList of string list
      (** The type of path target: 1. RootAccess -> i.e. "/", 2. PathList -> a
          list of paths *)

exception InvalidIp of string
exception OverlappingIps of string
exception InvalidTarget of string
exception EmptyHeaderList of string
exception EmptyCookieList of string

val make_traffic_filter_conf :
  filter_type:filter_type -> ?target:target_type -> unit -> traffic_filter_conf
(** [make_traffic_filter_conf m n o] makes a traffic filter configuration that
    takes a [m] filter type and a [n] target path *)

val make_traffic_filter :
  conf:traffic_filter_conf ->
  ?err_msg:string ->
  Dream.handler ->
  Dream.request ->
  Dream.response Lwt.t
(** [make_traffic_filter m  n] makes a traffic filter middleware that takes a
    [m] configuration, and a boolean [o] returns custom Unauthorized message *)
