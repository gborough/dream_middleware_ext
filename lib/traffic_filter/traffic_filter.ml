type traffic_filter_conf = { filter_type : filter_type; target : target_type }

and filter_type =
  | Ips of ip_filter
  | Headers of string list
  | Cookies of string list

and ip_filter = {
  whitelist : string list;
  blacklist : string list;
  x_real_ip : bool;
}

and target_type = RootAccess | PathList of string list

exception InvalidIp of string
exception OverlappingIps of string
exception InvalidTarget of string
exception EmptyHeaderList of string
exception EmptyCookieList of string

let is_elem s lst = List.mem s lst
let is_valid_ipv4_pat s = Ipaddr.of_string s |> Result.is_ok
let is_str_empty s = String.length s = 0

let is_valid_ipv4s l =
  if List.length l == 1 && (is_str_empty @@ List.hd l) then true
  else if List.for_all (fun elt -> is_valid_ipv4_pat elt) l then true
  else false

let has_x_real_ip req = Dream.has_header req "X-REAL-IP"
let get_x_real_ip req = Dream.header req "X-REAL-IP" |> Option.get
let split_ip ip = String.split_on_char ':' ip |> List.hd
let is_list_overlap wl bl = List.exists (fun elt -> List.mem elt wl) bl

let validate_target target =
  match target with
  | PathList lst ->
      if is_elem "/" lst then false
      else if List.length lst == 1 && (is_str_empty @@ List.hd lst) then false
      else true
  | _ -> true

let is_path path target =
  match target with
  | PathList lst -> if is_elem path lst then true else false
  | _ -> true

let make_traffic_filter_conf ~filter_type ?(target = RootAccess) () =
  if not @@ validate_target target then
    raise (InvalidTarget "Target path must not contain root `/` or is empty")
  else
    match filter_type with
    | Ips ips ->
        if not (is_valid_ipv4s ips.whitelist && is_valid_ipv4s ips.blacklist)
        then raise (InvalidIp "Invalid ip address pattern")
        else if is_list_overlap ips.whitelist ips.blacklist then
          raise (OverlappingIps "Whitelist and blacklist must not overlap")
        else { filter_type; target }
    | Headers h ->
        if List.exists (fun elt -> is_str_empty elt) h then
          raise (EmptyHeaderList "Header list must not be empty")
        else { filter_type; target }
    | Cookies c ->
        if List.exists (fun elt -> is_str_empty elt) c then
          raise (EmptyCookieList "Cookie list must not be empty")
        else { filter_type; target }

let make_traffic_filter ~conf ?(err_msg = "") =
 fun inner_handler req ->
  let ip = Dream.client req in
  let path = Dream.target req in
  match conf.filter_type with
  | Ips ips ->
      let real_ip =
        if ips.x_real_ip && has_x_real_ip req then get_x_real_ip req
        else split_ip ip
      in
      if is_elem real_ip ips.blacklist then
        Dream.respond ~status:`Unauthorized err_msg
      else if is_elem real_ip ips.whitelist && is_path path conf.target then
        inner_handler req
      else Dream.respond ~status:`Unauthorized err_msg
  | Headers h ->
      if
        List.exists (fun elt -> Dream.has_header req elt) h
        && is_path path conf.target
      then inner_handler req
      else Dream.respond ~status:`Unauthorized err_msg
  | Cookies c ->
      if
        List.exists (fun elt -> Dream.cookie req elt |> Option.is_some) c
        && is_path path conf.target
      then inner_handler req
      else Dream.respond ~status:`Unauthorized err_msg
