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
             if i == 0 then `Ok x.stdout else `BadExit (i, x.stderr)
           | `Signaled i ->
             `BadExit (i, x.stderr))
          |> Base.Or_error.return)
        ()
    in
    match out with
    | Error a ->
      Error (`EioErr a)
    | Ok (`Ok a) ->
      Ok a
    | Ok (`BadExit _ as a) ->
      Error a
  ;;

  (** Prevents concurrent acess to jj when running commands that cause snapshotting.
      jj can get currupted otherwise *)
  let access_lock = Eio.Mutex.create ()

  (** Run a jj command without outputting to the command_log.
      @param ?snapshot=true
        When true snapshots the state when running the command and also aquires a lock before running it. Set to false for commands you wish to run concurrently. like those for generating content in the UI
      @param ?color=true When true output will have terminal escape codes for color *)
  let jj_no_log_errorable ?(snapshot = true) ?(color = true) args =
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

  exception JJError of string
  (** Run a jj command without outputting to the command_log.
      @param ?snapshot=true
        When true snapshots the state when running the command and also aquires a lock before running it. Set to false for commands you wish to run concurrently. like those for generating content in the UI
      @param ?color=true When true output will have terminal escape codes for color *)
  let jj_no_log ?(snapshot = true) ?(color = true) args =

    match jj_no_log_errorable ~snapshot ~color args with
    | Ok a ->
      a
    | Error (`BadExit (code, str)) ->
      raise (JJError (Printf.sprintf "Exited with code %i; Message:\n%s" code str))
    | Error (`EioErr a) ->
      raise (JJError        (Printf.sprintf
           "Error running jj process:\n%a"
           (fun _ -> Base.Error.to_string_hum)
           a))
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
    let open Base.Result in
    let output =
      jj
        [
          "log"
        ; "--no-graph"
        ; "-T"
        ; {|"::"++current_working_copy++"::\n"++description++"\n::end::\n"|}
        ]
      |>String.trim
    in
    let current, prev = output |>Jj_tui.OutputParsing.parse_descriptions|>Result.get_ok in
    current |> String.concat "", prev |> String.concat ""
  ;;
end
