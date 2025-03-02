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
        let ret =
          Event.select
            [ Picos_io_select.on in_fd `R |> Event.map (fun x -> `Ready)
              (* This doesn't seem to be needed *)
              (* ; Picos_io_select.on in_fd `W |> Event.map (fun _ -> `Ready) *)
            ; Event.from_computation !computation |> Event.map (fun _ -> `LwdStateUpdate)
            ]
        in
        Printf.eprintf "done waiting\n";
        ret
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
    let trigger = ref (Computation.create ()) in
    let step = step trigger in_picos_fd in
    a := !a + 1;
    Ui_loop.Internal.run_with_term
      ~on_invalidate:(fun _ -> Computation.finish !trigger;)
      ~step
      term
  ;;

  let run = Ui_loop.Internal.run ~run_with_term ~tick_period:100.0
end
