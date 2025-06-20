(** Backend selection for mutex implementations. *)

module type MUTEX = sig
  type t
  val create : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
  val try_lock : t -> bool
  val protect : t -> (unit -> 'a) -> 'a
  val lock_all : t list -> bool
end

module Stdlib : MUTEX = struct
  include Mutex
  
  let lock_all mutexes =
    let rec try_lock_all acc = function
      | [] -> 
          (* All mutexes acquired successfully *)
          true
      | mutex :: rest ->
          if try_lock mutex then
            try_lock_all (mutex :: acc) rest
          else begin
            (* Failed to acquire current mutex, release all previously acquired ones *)
            List.iter unlock acc;
            false
          end
    in
    try_lock_all [] mutexes
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