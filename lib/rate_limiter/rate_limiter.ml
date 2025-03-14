module FixedWindow : sig
  type t
  type fw_conf
  type fw_store

  exception InvalidLimit of string
  exception InvalidTimeWindow of string

  val make_fw_conf : limit:int64 -> secs:float -> return_headers:bool -> fw_conf
  val make_fw_store : fw_conf -> fw_store

  val make_fw_rate_limiter :
    conf:fw_conf ->
    store:fw_store ->
    ?err_msg:string ->
    Dream.handler ->
    Dream.request ->
    Dream.response Lwt.t
end = struct
  type t = { capacity : int64; last_req : Ptime.Span.t }
  type fw_conf = { limit : int64; secs : float; return_headers : bool }
  type fw_store = t Lwt_mvar.t

  exception InvalidLimit of string
  exception InvalidTimeWindow of string

  let init_conf limit secs return_headers =
    if limit < 1L then
      raise (InvalidLimit "limit: Limit must be greater than 1")
    else if secs < 1. then
      raise
        (InvalidTimeWindow "secs: Time window must be greater than 1 second")
    else { limit; secs; return_headers }

  let init_store conf =
    Lwt_mvar.create { capacity = conf.limit; last_req = Ptime.Span.zero }

  let decrement_capacity prev_t t_mvar now =
    let new_limit = Int64.sub prev_t.capacity 1L in
    let new_t = { capacity = new_limit; last_req = Ptime.to_span now } in
    Lwt_mvar.put t_mvar new_t

  let limit_reached prev_t = if prev_t.capacity <= 0L then true else false

  let time_window_lapsed prev_t now conf =
    if
      Ptime.diff now @@ (Ptime.of_span prev_t.last_req |> Option.get)
      > (Ptime.Span.of_float_s conf.secs |> Option.get)
    then true
    else false

  let replenish t_mvar now conf =
    let new_last_req = Ptime.to_span now in
    let new_capacity = Int64.sub conf.limit 1L in
    let new_t = { capacity = new_capacity; last_req = new_last_req } in
    Lwt_mvar.put t_mvar new_t

  let make_fw_conf ~limit ~secs ~return_headers =
    init_conf limit secs return_headers

  let make_fw_store conf = init_store conf

  let make_fw_rate_limiter ~conf ~store ?(err_msg = "") =
   fun inner_handler req ->
    let prev_store = Lwt_mvar.take_available store |> Option.get in
    let now = Ptime_clock.now () in
    if time_window_lapsed prev_store now conf then
      let _ = replenish store now conf in
      inner_handler req
    else if limit_reached prev_store then
      let new_store = { capacity = 0L; last_req = prev_store.last_req } in
      let _ = Lwt_mvar.put store new_store in
      if conf.return_headers then
        let limit = conf.limit in
        let capacity = prev_store.capacity in
        let reset =
          Ptime.diff now @@ Option.get @@ Ptime.of_span prev_store.last_req
          |> Ptime.Span.to_float_s
        in
        let headers =
          [
            ("X-Rate-Limit-Limit", Int64.to_string limit);
            ("X-Rate-Limit-Remaining", Int64.to_string capacity);
            ("X-Rate-Limit-Reset", Float.to_string reset);
          ]
        in
        Dream.respond ~headers ~status:`Too_Many_Requests err_msg
      else Dream.respond ~status:`Too_Many_Requests err_msg
    else
      let _ = decrement_capacity prev_store store now in
      inner_handler req
end

module TokenBucket : sig
  type t
  type tb_conf
  type tb_bucket

  exception InvalidBucketSize of string
  exception InvalidRefillCount of string
  exception InvalidRefillRate of string

  val make_tb_conf :
    bucket_size:int64 -> refill_count:int64 -> refill_interval:float -> tb_conf

  val make_tb_bucket : tb_conf -> tb_bucket

  val make_tb_rate_limiter :
    conf:tb_conf ->
    bucket:tb_bucket ->
    ?err_msg:string ->
    Dream.handler ->
    Dream.request ->
    Dream.response Lwt.t
end = struct
  type t = { capacity : int64; last_req : Ptime.Span.t }

  type tb_conf = {
    bucket_size : int64;
    refill_count : int64;
    refill_interval : float;
  }

  type tb_bucket = t Lwt_mvar.t

  exception InvalidBucketSize of string
  exception InvalidRefillCount of string
  exception InvalidRefillRate of string

  let init_conf bucket_size refill_count refill_interval =
    if bucket_size < 1L then
      raise
        (InvalidBucketSize "bucket_size: Bucket size must be greater than 1")
    else if refill_interval < 1. then
      raise
        (InvalidRefillRate
           "refill_count: Refill rate must be greater than 1 second")
    else if refill_count < 1L || bucket_size < refill_count then
      raise
        (InvalidRefillCount
           "refill_interval: Refill count must be greater than 1 and smaller \
            than bucket size")
    else { bucket_size; refill_count; refill_interval }

  let init_bucket bucket_size =
    Lwt_mvar.create { capacity = bucket_size; last_req = Ptime.Span.zero }

  let refill_bucket t_mvar refill_count now =
    let new_capacity = Int64.sub refill_count 1L in
    let new_t = { capacity = new_capacity; last_req = Ptime.to_span now } in
    Lwt_mvar.put t_mvar new_t

  let reduce_token prev_t t_mvar now =
    let new_capacity = Int64.sub prev_t.capacity 1L in
    let new_t = { capacity = new_capacity; last_req = Ptime.to_span now } in
    Lwt_mvar.put t_mvar new_t

  let need_refill prev_t now conf =
    if
      Ptime.diff now @@ (Ptime.of_span prev_t.last_req |> Option.get)
      > (Ptime.Span.of_float_s conf.refill_interval |> Option.get)
    then true
    else false

  let bucket_empty prev_t = if prev_t.capacity <= 0L then true else false

  let make_tb_conf ~bucket_size ~refill_count ~refill_interval =
    init_conf bucket_size refill_count refill_interval

  let make_tb_bucket conf = init_bucket conf.bucket_size

  let make_tb_rate_limiter ~conf ~bucket ?(err_msg = "") =
   fun inner_handler req ->
    let prev_bucket = Lwt_mvar.take_available bucket |> Option.get in
    if prev_bucket.last_req == Ptime.Span.zero then
      let now = Ptime_clock.now () in
      let new_capacity = Int64.sub prev_bucket.capacity 1L in
      let new_store =
        { capacity = new_capacity; last_req = Ptime.to_span now }
      in
      let _ = Lwt_mvar.put bucket new_store in
      inner_handler req
    else
      let now = Ptime_clock.now () in
      if need_refill prev_bucket now conf then
        let _ = refill_bucket bucket conf.refill_count now in
        inner_handler req
      else if bucket_empty prev_bucket then
        let new_store = { capacity = 0L; last_req = prev_bucket.last_req } in
        let _ = Lwt_mvar.put bucket new_store in
        Dream.respond ~status:`Too_Many_Requests err_msg
      else
        let _ = reduce_token prev_bucket bucket now in
        inner_handler req
end
