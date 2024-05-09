open Eio

module type t = sig
  val jj : string list -> string
  val switch_to_process : Eio_unix.Stdenv.base -> string list -> Process.exit_status
end

module Make (Vars : Global_vars.Vars) = struct
  (** Makes a new process that has acess to all input and output
      This should be used for running other tui sub-programs *)
  let switch_to_process env command =
    Switch.run @@ fun sw ->
    let mgr = Eio.Stdenv.process_mgr env in
    let stdout = Eio.Stdenv.stdout env in
    let stdin = Eio.Stdenv.stdin env in
    let stderr = Eio.Stdenv.stderr env in
    let proc = Eio.Process.spawn ~sw mgr ~stderr ~stdin ~stdout command in
    proc |> Eio.Process.await
  ;;

  (* Ui_loop.run (Lwd.pure (W.printf "Hello world"));; *)
  let cmdArgs cmd args =
    let env = Vars.get_eio_env () in
    let mgr = Eio.Stdenv.process_mgr env in
    let cwd = Eio.Stdenv.cwd env in
    let out =
      Eio_process.run
        ~cwd
        ~process_mgr:mgr
        ~prog:cmd
        ~args
        ~f:(fun x ->
          (match x.exit_status with
           | `Exited i ->
             if i == 0 then x.stdout else x.stdout ^ x.stderr
           | `Signaled _ ->
             x.stderr)
          |> Base.Or_error.return)
        ()
    in
    out |> Result.to_option |> Option.value ~default:"there was an error"
  ;;

  let jj_no_log args =
    let res = cmdArgs "jj" (List.concat [ args; [ "--color"; "always" ] ]) in
    if res |> String.length > 10000
    then String.sub res 0 10000 ^ "...truncated because it's really long"
    else res
  ;;
  let jj args =
    (*update the command log*)
    let current_log = Lwd.peek Vars.ui_state.command_log in
    Lwd.set
      Vars.ui_state.command_log
      (([ "jj" ] @ args |> String.concat " ") :: current_log);

    jj_no_log args
  ;;
end
