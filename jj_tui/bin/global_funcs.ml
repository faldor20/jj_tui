open Global_vars
open Lwd_infix
open Jj_process.Make (Global_vars.Vars)
open Picos_std_structured

let colored_string = Jj_tui.AnsiReverse.colored_string

(**lists the files in a specific revision*)
let list_files ?(rev = "@") () =
  jj_no_log ~snapshot:false ~color:false [ "diff"; "-r"; rev; "--summary" ]
  |> String.split_on_char '\n'
  |> List.filter_map (fun x ->
    if x |> String.trim <> "" then Base.String.lsplit2 ~on:' ' x else None)
;;

let check_startup () =
  match jj_no_log_errorable ~color:false ~snapshot:false [  "op";"log"; "-l";"0" ] with
  | Ok _ ->
    `Good
  | Error (`BadExit (i, str)) ->
    if str |> Base.String.is_substring ~substring:"There is no jj repo"
    then `NotInRepo
    else `OtherError str
  | Error (`Exception e) ->
    `CantStartProcess e
;;

(**Updates the status windows; Without snapshotting the working copy by default
   This should be called after any command that performs a change *)
let update_status ?(update_graph = true) ?(cause_snapshot = false) () =
  safe_jj (fun () ->
    let rev = Lwd.peek Vars.ui_state.hovered_revision in
    let log_res = jj_no_log ~snapshot:cause_snapshot [ "log" ] |> colored_string in
    (* TODO: chagne this because it makes us always a frame behind *)
    if update_graph then Vars.ui_state.trigger_update $= ())
;;

(**Updates the status windows; Without snapshotting the working copy by default
   This should be called after any command that performs a change *)
let update_views ?(cause_snapshot = false) () =
  safe_jj (fun () ->
    let rev = Vars.get_hovered_rev () in
    let branches =
      jj_no_log ~snapshot:cause_snapshot [ "branch"; "list"; "-a" ] |> colored_string
    in
    (* From now on we use ignore-working-copy so we don't re-snapshot the state and so
       we can operate in paralell *)
    (* TODO: stop using dop last twice *)
    Show_view.re_render ();
    let files_list = Flock.fork_as_promise (fun _ -> list_files ~rev ()) in
    Vars.ui_state.jj_branches $= branches;
    (*wait for all our tasks*)
    let files_list = Promise.await files_list in
    (*now we can assign our results*)
    (* Vars.ui_state.jj_show $= log_res; *)
    Vars.ui_state.jj_change_files $= files_list)
;;
