(* route test a la Dream *)

let show ?(prefix = "/") ?(method_ = `GET) ?(headers = [ ("", "") ]) target
    router =
  try
    Dream.request ~method_ ~target ~headers "" |> Dream.test ~prefix router
    |> fun response ->
    let body =
      Dream.client_stream response
      |> Obj.magic |> Dream_pure.Stream.read_until_close |> Lwt_main.run
    in
    let status = Dream.status response in
    Printf.printf "Response: %i %s\n"
      (Dream.status_to_int status)
      (Dream.status_to_string status);
    if body <> "" then Printf.printf "%s\n" body else ()
  with Failure message -> print_endline message

let show_cors ?(prefix = "/") ?(method_ = `GET) ?(headers = [ ("", "") ]) target
    router =
  try
    Dream.request ~method_ ~target ~headers "" |> Dream.test ~prefix router
    |> fun response ->
    let body =
      Dream.client_stream response
      |> Obj.magic |> Dream_pure.Stream.read_until_close |> Lwt_main.run
    in
    let status = Dream.status response in
    let headers = Dream.all_headers response in
    Printf.printf "Response: %i %s\n"
      (Dream.status_to_int status)
      (Dream.status_to_string status);
    List.iter (fun (hrd, v) -> Printf.printf "%s: %s\n" hrd v) headers;
    if body <> "" then Printf.printf "%s\n" body else ()
  with Failure message -> print_endline message

let show_cors_non_preflight ?(prefix = "/") ?(method_ = `GET)
    ?(headers = [ ("", "") ]) target router =
  let open Dream_middleware_ext.Cors in
  try
    let req = Dream.request ~method_ ~target ~headers "" in
    let response = Dream.test ~prefix router req in
    let body =
      Dream.client_stream response
      |> Obj.magic |> Dream_pure.Stream.read_until_close |> Lwt_main.run
    in
    let status = Dream.status response in
    let headers = Dream.all_headers response in
    let non_preflight_headers = non_preflight_headers req in
    Printf.printf "Response: %i %s\n"
      (Dream.status_to_int status)
      (Dream.status_to_string status);
    List.iter (fun (hrd, v) -> Printf.printf "%s: %s\n" hrd v) headers;
    List.iter
      (fun (hrd, v) -> Printf.printf "%s: %s\n" hrd v)
      non_preflight_headers;
    if body <> "" then Printf.printf "%s\n" body else ()
  with Failure message -> print_endline message
