open Picos_std_structured
open Picos_std_sync
open Picos_std_finally
open Spawn

module type t = sig
  val jj : ?snapshot:bool -> string list -> string
  val switch_to_process : string list -> Unix.process_status
end

exception JJError of string * string

(* this mutex should be global*)
let access_lock = Mutex.create ()

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

  (*
     let read_fd_to_end fd =
     let buffer_size = 4096 in
     let buffer = Bytes.create buffer_size in
     let outBuf = Buffer.create 4096 in
     let rec read_all () =
     match Unix.read fd buffer 0 buffer_size with
     | 0 ->
     outBuf |> Buffer.to_bytes |> Bytes.to_string (* End of file *)
     | n ->
     read_all (Buffer.add_bytes outBuf (Bytes.sub buffer 0 n))
     in
     read_all ()
     ;;

     let picos_process cmd args =
     let stdout_r, stdout_w = Unix.pipe () in
     let stdin_r, stdin_w = Unix.pipe () in
     let stderr_r, stderr_w = Unix.pipe () in
     let pid =
     Unix.create_process_env
     cmd
     (Array.of_list (cmd :: args))
     (Unix.environment ())
     stdin_r
     stdout_w
     stderr_w
     in
     (* Close unused pipe ends in the parent process *)
     Unix.close stdout_w;
     Unix.close stdin_r;
     Unix.close stderr_w;
     (* Read output in a non-blocking way *)
     Unix.set_nonblock stdout_r;
     Unix.set_nonblock stderr_r;
     let rec collect_output out err =
     try
     let stdout = read_fd_to_end stdout_r in
     let stderr = read_fd_to_end stderr_r in
     out ^ stdout, err ^ stderr
     with
     | Unix.Unix_error (Unix.EAGAIN, _, _) ->
     Unix.sleepf 0.01;
     (* Short sleep to avoid busy waiting *)
     collect_output out err
     in
     let stdout, stderr = collect_output "" "" in
     let code, status = Unix.waitpid [] pid in
     (* Close remaining pipe ends *)
     Unix.close stdout_r;
     Unix.close stdin_w;
     Unix.close stderr_r;
     code, status, stdout, stderr
     ;;
  *)

  let read_fd_to_end fd =
    let open Picos_io in
    Flock.fork_as_promise (fun () ->
      let buffer_size = 4096 in
      let buffer = Bytes.create buffer_size in
      let rec read_loop acc =
        try
          match Picos_io.Unix.read fd buffer 0 buffer_size with
          | 0 ->
            String.concat "" (List.rev acc) (* EOF reached *)
          | n ->
            read_loop (Bytes.sub_string buffer 0 n :: acc)
        with
        | Unix.Unix_error (Unix.EBADF, _, _) ->
          String.concat "" (List.rev acc)
        (* Handle EBADF error *)
      in
      read_loop [])
  ;;

  let picos_process cmd args =
    let open Picos_io in
    let@ stdout_o, stdout_i =
      finally (fun (o, i) -> Unix.close o) (Picos_io.Unix.pipe ~cloexec:true)
    in
    let@ stdin_o, stdin_i =
      finally (fun (o, i) -> Unix.close i) (Picos_io.Unix.pipe ~cloexec:true)
    in
    let@ stderr_o, stderr_i =
      finally (fun (o, i) -> Unix.close o) (Picos_io.Unix.pipe ~cloexec:true)
    in
    (* This should ensure that all children processes are killed before we cleanup the pipes*)
    Flock.join_after @@ fun () ->
    let pid =
          Picos_io.Unix.create_process_env
            cmd
            (cmd :: args |> Array.of_list)
            (Unix.environment ())
            stdin_o
            stdout_i
            stderr_i
    in
    let prom = Flock.fork_as_promise (fun () -> Picos_io.Unix.waitpid [] pid) in
    (* Close unused pipe ends in the parent process *)
    Unix.close stdout_i;
    Unix.close stdin_o;
    Unix.close stderr_i;
    Unix.set_nonblock stdout_o;
    Unix.set_nonblock stderr_o;
    let stdout_prom = read_fd_to_end stdout_o in
    let stderr_prom = read_fd_to_end stderr_o in
    let stdout = Promise.await stdout_prom in
    let stderr = Promise.await stderr_prom in
    let code, status = Promise.await prom in
    (* let stderr = read_fd_to_end stderr_i in *)
    (* let stdout= ""in *)
    code, status, stdout, stderr
  ;;

  (* Ui_loop.run (Lwd.pure (W.printf "Hello world"));; *)
  let cmdArgs cmd args =
    let code, status, out_content, err_content = picos_process cmd args in
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
      | Picos_std_structured.Control.Terminate as e ->
        raise e
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
  let jj_no_log ?(get_stderr = false) ?(snapshot = false) ?(color = true) args =
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

  let jj ?(snapshot = true) args =
    (*update the command log*)
    let current_log = Lwd.peek Vars.ui_state.command_log in
    Lwd.set
      Vars.ui_state.command_log
      (([ "jj" ] @ args |> String.concat " ") :: current_log);
    jj_no_log ~snapshot args
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
