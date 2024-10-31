open Notty
open Nottui
open Picos
open Picos_std_structured
open  Picos_std_finally

module Ui_loop = struct
let step in_fd =
  Ui_loop.Internal.step ~await_read:(fun _ timeout ->
    (*await the read inside a promise*)
    let rec select ()=
      match Picos_io.Unix.select [in_fd] [] [in_fd ] timeout with
      | [], [], [] -> `NotReady
      | _ -> `Ready
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> select ()

      in
    select ()
    )
;;

let run_with_term term =
  let in_fd,out_fd= Notty_unix.Term.fds term in
  let in_fd=Picos_io_fd.create in_fd in
  let step= step in_fd in
   
  Ui_loop.Internal.run_with_term ~step term
;;


let run = Ui_loop.Internal.run ~run_with_term  
end
