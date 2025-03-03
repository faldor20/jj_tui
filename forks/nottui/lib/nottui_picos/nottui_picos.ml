open Notty
open Nottui
open Picos
open Picos_std_structured
open Picos_std_finally
open Picos_std_event
open Picos_std_sync
open Notty
open Notty_unix

(*Super simple method for tracking invalidations that occur outside of a computation using picos.
We already track and apply invaldations that happen within a ui recompute
*)
module InvalidationTracker = struct
  type t = unit Picos.Computation.t ref

  let start_tracking tracker = tracker := Computation.create ()
  let create () : t = Computation.create () |> ref
  let invalidated_evt tracker = Event.from_computation !tracker
  let invalidate (tracker : t) = Computation.finish !tracker
end

module It = InvalidationTracker

module Ui_loop = struct
  let wait_for_event invalidation_tracker in_fd () =
    let rec select () =
      let ret =
        Event.select
          [ Picos_io_select.on in_fd `R |> Event.map (fun x -> `Ready)
            (* This doesn't seem to be needed *)
            (* ; Picos_io_select.on in_fd `W |> Event.map (fun _ -> `Ready) *)
            (*If our compution*)
          ; It.invalidated_evt invalidation_tracker
            |> Event.map (fun _ -> `LwdStateUpdate)
          ]
       in
      ret
    in
    select ()
  ;;

  let step
        ?(process_event = true)
        ?(timeout = -1.0)
        ~awaiter
        ~invalidation_tracker
        ~renderer
        ~cache
        term
        root
    =
    let size = Term.size term in
    let image =
      if (not (Lwd.is_damaged root)) && !cache |> Option.is_some
      then !cache |> Option.get
      else (
        let rec stabilize () =
          let tree = Lwd.quick_sample root in
          Renderer.update renderer size tree;
          It.start_tracking invalidation_tracker;
          let image = Renderer.image renderer in
          (* If we are already damaged then we should re-calculate*)
          if Lwd.is_damaged root then stabilize () else image
        in
        stabilize ())
    in
    cache := Some image;
    Term.image term image;
    (* Now we wait for another event or the timeout*)
    if process_event
    then (
      let wait_for_event () =
        match awaiter () with
        | `NotReady -> false
        | `Ready -> true
        | `LwdStateUpdate -> false
      in
      (* for async I should extend this to include changed lwd.var values*)
      (* let has_event =Term.pending term  in *)
      if wait_for_event ()
      then (
        match Term.event term with
        | `End -> ()
        | `Resize _ -> ()
        | #Unescape.event as event ->
          let event = (event : Unescape.event :> Ui.event) in
          ignore (Renderer.dispatch_event renderer event : [> `Handled | `Unhandled ])))
  ;;

  let a = ref 0

  let run_with_term term ?on_invalidate =
    let in_fd, out_fd = Notty_unix.Term.fds term in
    (* the term will likely be attached to stdin so we check to make sure we don't recreate that file handle, because picos creates this handle at startup*)
    let in_picos_fd =
      if in_fd = (Picos_io.Unix.stdin |> Picos_io_fd.unsafe_get)
      then Picos_io.Unix.stdin
      else Picos_io_fd.create ~dispose:false in_fd
    in
    let invalidation_tracker = It.create () in
    (* let step = step trigger in_picos_fd in *)
    a := !a + 1;
    let cache = ref None in
    Ui_loop.Internal.run_with_term
      ~on_invalidate:(fun _ -> It.invalidate invalidation_tracker)
      ~step:
        (step
           ~awaiter:(wait_for_event invalidation_tracker in_picos_fd)
           ~invalidation_tracker)
      term
  ;;

  let run = Ui_loop.Internal.run ~run_with_term ~tick_period:0.01
end
