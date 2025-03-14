(** Delay middleware simulating delayed request *)

val make_delay :
  secs:float -> Dream.handler -> Dream.request -> Dream.response Lwt.t
(** [make_delay m] delays request for [m] seconds *)
