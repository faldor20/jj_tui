(** Backend selection for mutex implementations. *)

module type MUTEX = sig
  type t
  val create : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
  val try_lock : t -> bool
  val protect : t -> (unit -> 'a) -> 'a
end

module Stdlib : MUTEX = struct
  include Mutex
  

end

(* Picos implementation - only available if picos is linked *)
module Picos : MUTEX = struct
  (* This is a placeholder implementation that will be replaced
     when picos is available. For now, it falls back to stdlib. *)
  include Stdlib
end

(* Default backend - can be changed at compile time *)
module Default = Stdlib

(* Functor to create a mutex module from any backend *)
module Make (Backend : MUTEX) = Backend 