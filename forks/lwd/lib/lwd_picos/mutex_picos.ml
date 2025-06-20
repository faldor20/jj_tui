
(** Picos implementation of the mutex interface. *)

(* Note: This implementation requires the picos library to be available.
   You may need to add picos as a dependency in your dune-project file. *)

(* We'll use Picos's synchronization primitives to implement mutexes.
   Since Picos doesn't have a direct mutex equivalent, we'll implement
   one using Picos's basic synchronization primitives. *)

include Picos_std_sync.Mutex

let create () =  create ()

let lock mut = lock mut

let unlock mut = unlock mut
let try_lock mut = try_lock mut
let protect mut f = protect mut f

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
