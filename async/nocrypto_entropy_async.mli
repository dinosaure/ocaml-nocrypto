open Core
open Async

val initialize: unit -> unit Deferred.t

type t

val attach: period:Core.Time_ns.Span.t -> ?device:string -> Nocrypto.Rng.g -> t Deferred.t
val stop: t -> unit Deferred.t
