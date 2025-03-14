let make_delay ~secs =
 fun inner_handler req ->
  let%lwt _ = Lwt_unix.sleep secs in
  inner_handler req
