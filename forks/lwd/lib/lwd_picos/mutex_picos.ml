
(** Picos implementation of the mutex interface. *)

(* Note: This implementation requires the picos library to be available.
   You may need to add picos as a dependency in your dune-project file. *)

(* We'll use Picos's synchronization primitives to implement mutexes.
   Since Picos doesn't have a direct mutex equivalent, we'll implement
   one using Picos's basic synchronization primitives. *)

type t = {
  mutable locked : bool;
  mutable owner : Picos.Fiber.t option;
  waiters : Picos.Trigger.t list ref;
}

let create () = {
  locked = false;
  owner = None;
  waiters = ref [];
}

let lock mutex =
  let rec try_acquire () =
    if not mutex.locked then begin
      mutex.locked <- true;
      mutex.owner <- Some (Picos.Fiber.current ());
    end else begin
      (* Create a trigger to wait for the mutex to be released *)
      let trigger = Picos.Trigger.create () in
      mutex.waiters := trigger :: !(mutex.waiters);
      match Picos.await trigger with
      | None -> (* Cancelled *) raise (Sys_error "Mutex lock cancelled")
      | Some (exn, _) -> raise exn
    end
  in
  try_acquire ()

let try_lock mutex =
  if not mutex.locked then begin
    mutex.locked <- true;
    mutex.owner <- Some (Picos.Fiber.current ());
    true
  end else
    false

let unlock mutex =
  if not mutex.locked then
    raise (Sys_error "Mutex is not locked")
  else
    let current_fiber = Picos.Fiber.current () in
    match mutex.owner with
    | None -> raise (Sys_error "Mutex has no owner")
    | Some owner ->
        if not (Picos.Fiber.equal current_fiber owner) then
          raise (Sys_error "Mutex was locked by another fiber")
        else begin
          mutex.locked <- false;
          mutex.owner <- None;
          (* Wake up one waiter if any *)
          match !(mutex.waiters) with
          | [] -> ()
          | trigger :: rest ->
              mutex.waiters := rest;
              Picos.Trigger.signal trigger ()
        end

let protect mutex f =
  lock mutex;
  try
    let result = f () in
    unlock mutex;
    result
  with exn ->
    unlock mutex;
    raise exn

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