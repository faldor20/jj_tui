open Notty
open Nottui
open Picos
open Picos_std_structured
open Picos_std_finally
open Picos_std_event
open Picos_std_sync

module Ui_loop = struct
  let step computation in_fd =
    Ui_loop.Internal.step ~await_read:(fun _ timeout ->
      let rec select () =
        Printf.eprintf "waiting on events\n";
        computation := Ivar.create ();
        let cancelEvent = Ivar.read_evt !computation in
        let ret =
          Event.select
            [ Picos_io_select.on in_fd `R |> Event.map (fun x -> `Ready)
            ; Picos_io_select.on in_fd `W |> Event.map (fun _ -> `Ready)
            ; cancelEvent
              |> Event.map (fun x ->
                Printf.eprintf "rerun-invalidation\n";
                `LwdStateUpdate)
            ; Picos_io_select.timeout ~seconds:10.0
              |> Event.map (fun x -> `NotReady)
            ]
        in
        Printf.eprintf "finished waiting\n";
        ret
        (* match Picos_io.Unix.select [ in_fd ] [] [ in_fd ] timeout with *)
        (* | [], [], [] -> `NotReady *)
        (* | _ -> `Ready *)
        (* | exception Unix.Unix_error (Unix.EINTR, _, _) -> select () *)
      in
      select ())
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
    let trigger = ref (Ivar.create ()) in
    let step = step trigger in_picos_fd in
    a := !a + 1;
    Ui_loop.Internal.run_with_term
      ~on_invalidate:(fun _ ->
        Printf.eprintf "invalidated\n";
        Ivar.fill !trigger ())
      ~step
      term
  ;;

  let run = Ui_loop.Internal.run ~run_with_term
end
