(** Rate limiter middleware *)

(** Fixed Window algorithm. *)
module FixedWindow : sig
  type t
  (** The type of fixed window *)

  type fw_conf
  (** The type of fixed window configuration *)

  type fw_store
  (** The type of fixed window container *)

  exception InvalidLimit of string
  exception InvalidTimeWindow of string

  val make_fw_conf : limit:int64 -> secs:float -> return_headers:bool -> fw_conf
  (** [make_fw_conf m n o] makes a configuration of window of [n] seconds which
      contains [m] request permits, and boolean [o] returns X-Rate-Limit-*
      headers conditionally *)

  val make_fw_store : fw_conf -> fw_store
  (** [make_fw_store conf] makes a global fixed window container *)

  val make_fw_rate_limiter :
    conf:fw_conf ->
    store:fw_store ->
    ?err_msg:string ->
    Dream.handler ->
    Dream.request ->
    Dream.response Lwt.t
  (** [make_fw_rate_limiter m n o] makes a fixed window middleware that takes a
      [m] configuration and a [n] global fixed windown container, and a boolean
      [o] returns custom Too_Many_Requests message *)
end

(** Token Bucket algorithm. *)
module TokenBucket : sig
  type t
  (** The type of token bucket *)

  type tb_conf
  (** The type of token bucket configuration *)

  type tb_bucket
  (** The type of token bucket container *)

  exception InvalidBucketSize of string
  exception InvalidRefillCount of string
  exception InvalidRefillRate of string

  val make_tb_conf :
    bucket_size:int64 -> refill_count:int64 -> refill_interval:float -> tb_conf
  (** [make_tb_conf m n o] makes a configuration of initial bucket size [m], and
      refills the bucket for [n] counts every [o] seconds *)

  val make_tb_bucket : tb_conf -> tb_bucket
  (** [make_tb_bucket conf] makes a global token bucket *)

  val make_tb_rate_limiter :
    conf:tb_conf ->
    bucket:tb_bucket ->
    ?err_msg:string ->
    Dream.handler ->
    Dream.request ->
    Dream.response Lwt.t
  (** [make_tb_rate_limiter m n o] makes a token bucket middleware that takes a
      [m] configuration and a [n] global token bucket, and a boolean [o] returns
      custom Too_Many_Requests message *)
end
