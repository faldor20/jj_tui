open Picos_std_structured

type 'a t = {
    delay : float
  ; merge : 'a -> 'a -> 'a
  ; run : 'a -> unit
  ; pending : 'a option ref
  ; debounce_computation : unit Promise.t ref
  ; current_computation : unit Promise.t ref
}

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
