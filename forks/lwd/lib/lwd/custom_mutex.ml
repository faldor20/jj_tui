module type MUTEX = sig
(** Locks for mutual exclusion with support for multiple concurrency backends.

   This module provides a unified interface for mutexes that can work with
   different concurrency backends (OCaml standard library, Picos, etc.).
   It extends the standard OCaml mutex interface with additional functionality
   for acquiring multiple mutexes atomically.
*)

type t
(** The type of mutexes. *)

val create : unit -> t
(** Return a new mutex. *)

val lock : t -> unit
(** Lock the given mutex. Only one thread can have the mutex locked
   at any time. A thread that attempts to lock a mutex already locked
   by another thread will suspend until the other thread unlocks
   the mutex.

   @raise Sys_error if the mutex is already locked by the thread calling
   {!lock}.

   @before 4.12 {!Sys_error} was not raised for recursive locking
   (platform-dependent behaviour) *)

val try_lock : t -> bool
(** Same as {!lock}, but does not suspend the calling thread if
   the mutex is already locked: just return [false] immediately
   in that case. If the mutex is unlocked, lock it and
   return [true]. *)

val unlock : t -> unit
(** Unlock the given mutex. Other threads suspended trying to lock
   the mutex will restart.  The mutex must have been previously locked
   by the thread that calls {!unlock}.
   @raise Sys_error if the mutex is unlocked or was locked by another thread.

   @before 4.12 {!Sys_error} was not raised when unlocking an unlocked mutex
   or when unlocking a mutex from a different thread. *)

val protect : t -> (unit -> 'a) -> 'a
(** [protect mutex f] runs [f()] in a critical section where [mutex]
    is locked (using {!lock}); it then takes care of releasing [mutex],
    whether [f()] returned a value or raised an exception.

    The unlocking operation is guaranteed to always takes place,
    even in the event an asynchronous exception (e.g. {!Sys.Break}) is raised
    in some signal handler.

    @since 5.1 *)

val lock_all : t list -> bool
(** [lock_all mutexes] attempts to acquire all mutexes in the list atomically.
    It uses {!try_lock} for each mutex in the order provided. If any mutex
    cannot be acquired, it releases all previously acquired mutexes and
    returns [false]. If all mutexes are successfully acquired, it returns [true].
    
    This function is useful for avoiding deadlocks when multiple mutexes
    need to be acquired simultaneously.
    
    @return [true] if all mutexes were successfully acquired, [false] otherwise.
    
    Note: The caller is responsible for unlocking all mutexes that were
    successfully acquired when this function returns [true]. *) 
end