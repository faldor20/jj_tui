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
    let Vars.{ cwd; mgr; _ } = Vars.get_eio_vars () in
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

  (** Prevents concurrent acess to jj when running commands that cause snapshotting.
      jj can get currupted otherwise *)
  let access_lock = Eio.Mutex.create ()

  (** Run a jj command without outputting to the command_log.
      @param ?snapshot=true
        When true snapshots the state when running the command and also aquires a lock before running it. Set to false for commands you wish to run concurrently. like those for generating content in the UI
      @param ?color=true When true output will have terminal escape codes for color *)
  let jj_no_log ?(snapshot = true) ?(color = true) args =
    let locked =
      if snapshot
      then (
        Mutex.lock access_lock;
        true)
      else false
    in
    let res =
      cmdArgs
        "jj"
        (List.concat
           [
             args
           ; (if snapshot then [] else [ "--ignore-working-copy" ])
           ; (if color then [ "--color"; "always" ] else [ "--color"; "never" ])
           ])
    in
    if locked then Mutex.unlock access_lock;
    res
  ;;

  let jj args =
    (*update the command log*)
    let current_log = Lwd.peek Vars.ui_state.command_log in
    Lwd.set
      Vars.ui_state.command_log
      (([ "jj" ] @ args |> String.concat " ") :: current_log);
    jj_no_log args
  ;;

  (**gets the description of the current and previous change. Useful when squashing*)
  let get_messages () =
    let output =
      jj
        [
          "log"
        ; "--no-graph"
        ; "-T"
        ; {|"::"++current_working_copy++"::\n"++description++"\n::end::\n"|}
        ]
      |> String.trim
    in
    let current, prev =
      output |> Jj_tui.OutputParsing.parse_descriptions |> Result.get_ok
    in
    current |> String.concat "", prev |> String.concat ""
  ;;
end
