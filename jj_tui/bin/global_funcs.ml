open Global_vars
open Lwd_infix
open Jj_process.Make (Global_vars.Vars)

let colored_string = Jj_tui.AnsiReverse.colored_string

(**lists the files in a specific revision*)
let list_files ?(rev = "@") () =
  jj_no_log ~snapshot:false ~color:false [ "diff"; "-r"; rev; "--summary" ]
  |> String.split_on_char '\n'
  |> List.filter_map (fun x ->
    if x |> String.trim <> ""
    then (
      match String.split_on_char ' ' x with
      | [ a; b ] ->
        Some (a, b)
      | _ ->
        failwith
          "Getting files should always return a list of file names with a modifier at \
           the start ")
    else None)
;;


(**Updates the state; Without snapshotting the working copy by default *)
let on_change ?(cause_snapshot = false) () =
  Eio.Switch.run @@ fun sw ->
  let log_res =
    jj_no_log ~snapshot:cause_snapshot [ "show"; "-s"; "--color-words" ] |> colored_string
  in
  (* From now on we use ignore-working-copy so we don't re-snapshot the state and so
     we can operate in paralell *)
  let tree =
    Eio.Fiber.fork_promise ~sw (fun _ ->
      jj_no_log ~snapshot:false [ "log" ] |> colored_string)
  (* TODO: stop using dop last twice *)
  and branches =
    Eio.Fiber.fork_promise ~sw (fun _ ->
      jj_no_log ~snapshot:false [ "branch"; "list"; "-a" ] |> colored_string)
  and files_list =
    Eio.Fiber.fork_promise ~sw (fun _ ->
  list_files ())
  in
  (*wait for all our tasks*)
  let tree = Eio.Promise.await_exn tree
  and files_list = Eio.Promise.await_exn files_list
  and branches = Eio.Promise.await_exn branches in
  (*now we can assign our results*)
  Vars.render_mutex|>Eio.Mutex.lock;

  Vars.ui_state.jj_show $= log_res;
  Vars.ui_state.jj_branches $= branches;
  Vars.ui_state.jj_tree $= tree;
  Vars.ui_state.jj_change_files $= files_list;

  Vars.render_mutex|>Eio.Mutex.unlock;
;;
