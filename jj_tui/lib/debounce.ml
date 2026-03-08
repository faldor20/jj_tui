open Picos_std_structured

(**
   A small, reusable debouncer for Picos fibers.

   The debouncer coalesces rapid [push] calls into a single delayed execution.
   Callers provide a [merge] function to combine queued values and a [run]
   function for the final work item.

   Internally, both the debounce timer and the active run are represented as
   cancelable promises. This keeps latest-wins behavior explicit and avoids
   leaking background work.

   Warning:
   - This module uses [Flock.fork_as_promise], so calls to {!make} and {!push}
     must happen inside a running flock scope.
   - The [run] callback should not swallow [Control.Terminate], otherwise
     cancellation cannot stop stale work promptly.
*)
type 'a t = {
    delay : float
  ; merge : 'a -> 'a -> 'a
  ; run : 'a -> unit
  ; pending : 'a option ref
  ; debounce_computation : unit Promise.t ref
  ; current_computation : unit Promise.t ref
}

(** [make ~delay ~merge ~run ()] creates a debouncer.

    - [delay] is the debounce window in seconds.
    - [merge old new_] combines queued values when multiple pushes arrive.
    - [run value] performs the debounced action.

    [run] executes in a fiber forked from the current flock.
*)
let make ~delay ~merge ~run () =
  {
    delay
  ; merge
  ; run
  ; pending = ref None
  ; debounce_computation = ref (Promise.of_value ())
  ; current_computation = ref (Promise.of_value ())
  }
;;

(** [push t value] enqueues a new value for debounced processing.

    Repeated pushes during the debounce window are merged with [merge]. When the
    timer elapses, only the latest merged value is executed. Any previously
    running [run] fiber is canceled before a new one is started.
*)
let push t value =
  t.pending
  := Some
       (match !(t.pending) with None -> value | Some pending -> t.merge pending value);
  Promise.terminate !(t.debounce_computation);
  t.debounce_computation
  := Flock.fork_as_promise (fun () ->
       Control.sleep ~seconds:t.delay;
       match !(t.pending) with
       | None ->
         ()
       | Some pending ->
         t.pending := None;
         Promise.terminate !(t.current_computation);
         t.current_computation := Flock.fork_as_promise (fun () -> t.run pending))
;;
