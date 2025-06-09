(** OCaml standard library implementation of the mutex interface. *)

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