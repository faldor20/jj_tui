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
  match jj_no_log_errorable ~color:false [ "log"; "''" ] with
  | Ok _ ->
    `Good
  | Error (`BadExit (i, str)) ->
    if str |> Base.String.is_substring ~substring:"There is no jj repo"
    then `NotInRepo
    else `OtherError str
  | Error (`Exception _) ->
    `CantStartProcess
;;

(**Updates the status windows; Without snapshotting the working copy by default
   This should be called after any command that performs a change *)
let update_status ?(update_graph = true) ?(cause_snapshot = false) () =
  safe_jj (fun () ->
    let rev = Lwd.peek Vars.ui_state.selected_revision in
    let log_res = jj_no_log ~snapshot:cause_snapshot [ "log" ] |> colored_string in
    if update_graph then Vars.ui_state.trigger_update $= ())
;;

(**Updates the status windows; Without snapshotting the working copy by default
   This should be called after any command that performs a change *)
let update_views ?(cause_snapshot = false) () =
  safe_jj (fun () ->
    let rev = Vars.get_selected_rev () in
    Flock.join_after @@ fun () ->
    let tree =
      jj_no_log ~snapshot:cause_snapshot [ "log"; "-r"; rev ] |> colored_string
    in
    (* From now on we use ignore-working-copy so we don't re-snapshot the state and so
       we can operate in paralell *)
    (* TODO: stop using dop last twice *)
    let _=
      (* TODO: could these all just run in fully paralell like this ?*)
      !(Vars.ui_state.jj_show_promise) |> Promise.terminate;
      Vars.ui_state.jj_show_promise
      := Flock.fork_as_promise @@ fun () ->
         let show_data =
           jj_no_log ~snapshot:false [ "show"; "-s"; "--color-words"; "-r"; rev ]
           |> colored_string
         in
         Vars.ui_state.jj_show $= show_data
    and branches =
      Flock.fork_as_promise (fun _ ->
        jj_no_log ~snapshot:false [ "branch"; "list"; "-a" ] |> colored_string)
    and files_list = Flock.fork_as_promise (fun _ -> list_files ~rev ()) in
    (*wait for all our tasks*)
    let files_list = Promise.await files_list
    and branches = Promise.await branches in
    (*now we can assign our results*)
    (* Vars.ui_state.jj_show $= log_res; *)
    Vars.ui_state.jj_branches $= branches;
    Vars.ui_state.jj_tree $= tree;
    Vars.ui_state.jj_change_files $= files_list)
;;
