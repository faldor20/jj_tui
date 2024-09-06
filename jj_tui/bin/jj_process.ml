open Picos_std_structured
open Picos_std_sync
open Picos_std_finally

module type t = sig
  val jj : string list -> string
  val switch_to_process : string list -> Unix.process_status
end

exception JJError of string * string

module Make (Vars : Global_vars.Vars) = struct
  (** Makes a new process that has acess to all input and output
      This should be used for running other tui sub-programs *)
  let switch_to_process command =
    let stdout = Unix.stdout in
    let stdin = Unix.stdin in
    let stderr = Unix.stderr in
    let pid = Unix.create_process command.(0) command stdin stdout stderr in
    let _, status = Unix.waitpid [] pid in
    status
  ;;

  (* Ui_loop.run (Lwd.pure (W.printf "Hello world"));; *)
  let cmdArgs cmd args =
    let stdout, stdin, stderr =
      Unix.open_process_args_full cmd (Array.of_list (cmd::args)) (Unix.environment ())
    in
    let out_content = In_channel.input_all stdout in
    let err_content = In_channel.input_all stderr in
    (* TODO: may need to wait before calling close*)
    let status = Unix.close_process_full (stdout, stdin, stderr) in
    let exit_code =
      match status with
      | Unix.WEXITED code ->
        code
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        -1
    in
    match exit_code with
    | 0 ->
      Ok (out_content, err_content)
    | _ ->
      Error (`BadExit (exit_code, err_content ^ "\n" ^ out_content))
  ;;

  (** Prevents concurrent acess to jj when running commands that cause snapshotting.
      jj can get currupted otherwise *)
  let access_lock = Mutex.create ()

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
      try
        cmdArgs
          "jj"
          (List.concat
             [
               args
             ; (if snapshot then [] else [ "--ignore-working-copy" ])
             ; (if color then [ "--color"; "always" ] else [ "--color"; "never" ])
             ])
      with
      | e ->
        Error (`Exception (Printexc.to_string e))
    in
    if locked then Mutex.unlock access_lock;
    res
  ;;

  (** Run a jj command without outputting to the command_log.
      @param ?snapshot=true
        When true snapshots the state when running the command and also aquires a lock before running it. Set to false for commands you wish to run concurrently. like those for generating content in the UI
      @param ?color=true When true output will have terminal escape codes for color *)
  let jj_no_log ?(get_stderr = false) ?(snapshot = true) ?(color = true) args =
    match jj_no_log_errorable ~snapshot ~color args with
    | Ok a ->
      if get_stderr then a |> snd else a |> fst
    | Error (`BadExit (code, str)) ->
      raise
        (JJError
           ( "jj" :: args |> String.concat " "
           , Printf.sprintf "Exited with code %i; Message:\n%s" code str ))
    | Error (`Exception a) ->
      raise
        (JJError
           ( "jj" :: args |> String.concat " "
           , Printf.sprintf "Error running jj process:\n%s" a ))
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
  let get_messages source dest =
    let open Base.Result in
    let output =
      jj
        [
          "log"
        ; "--no-graph"
        ; "-T"
        ; Printf.sprintf
            {|if(self.contained_in("%s")||self.contained_in("%s"),description++"%s")++if(self.contained_in("%s")||self.contained_in("%s"),description)|}
            source
            source
            "\u{ab}"
            dest
            dest
        ]
      |> String.trim
    in
    let source, dest = output |> Base.String.lsplit2_exn ~on:'\xab' in
    Base.String.drop_suffix source 1, dest
  ;;

  open Vars
  open Nottui
  open Lwd_infix

  (**handle exception from jj by showing an error message*)
  let handle_jj_error ~cmd ~error =
    ui_state.show_prompt $= None;
    ui_state.show_popup
    $= Some
         ( error
           |> Jj_tui.AnsiReverse.colored_string
           |> Ui.atom
           |> Ui.resize ~sw:1 ~sh:1
           |> Lwd.pure
         , Printf.sprintf "An error occured running %s" cmd );
    ui_state.input $= `Mode (fun _ -> `Unhandled)
  ;;

  (*catch any exceptions from jj*)
  let safe_jj f = try f () with JJError (cmd, error) -> handle_jj_error ~cmd ~error
end
