open Global_vars
open Lwd_infix
open Jj_process.Make (Global_vars.Vars)
open Picos_std_structured
open Jj_tui.Logging

let colored_string = Jj_tui.AnsiReverse.colored_string

(**lists the files in a specific revision*)
let list_files ?(rev = "@") () =
  jj_no_log ~snapshot:false ~color:false [ "diff"; "-r"; rev; "--summary" ]
  |> String.split_on_char '\n'
  |> List.filter_map (fun x ->
    if x |> String.trim <> "" then Base.String.lsplit2 ~on:' ' x else None)
;;

let check_startup () =
  let result = Lwd.var `Good in
  (* In the happy path making this a fiber and returning an Lwd.t makes this not delay rendering. In the unhappy path, after 150-50ms the error message will show*)
  Flock.fork (fun x ->
    let res =
      match
        (*we snapshot here in the first request to make sure the editor is showing the latest changes*)
        jj_no_log_errorable ~color:false ~snapshot:true [ "op"; "log"; "--limit"; "0" ]
      with
      | Ok _ ->
        `Good
      | Error (`BadExit (i, str)) ->
        if str |> Base.String.is_substring ~substring:"There is no jj repo"
        then `NotInRepo
        else `OtherError str
      | Error (`Exception e) ->
        `CantStartProcess e
    in
    (*we don't want to trigger a re-render if the result is still good *)
    if res != Lwd.peek result then result $= res);
  result |> Lwd.get
;;

(**Updates the status windows; Without snapshotting the working copy by default
   This should be called after any command that performs a change *)
let update_views ?(cause_snapshot = false) () =
  safe_jj (fun () ->
    [%log debug "updating views"];
    let rev = Vars.get_hovered_rev () in
    let branches =
      jj_no_log ~snapshot:cause_snapshot [ "bookmark"; "list"; "-a" ] |> colored_string
    in
    (* From now on we use ignore-working-copy so we don't re-snapshot the state and so
       we can operate in paralell *)
    (* TODO: stop using dop last twice *)
    Show_view.re_render ();
    let files_list = Flock.fork_as_promise (fun _ -> list_files ~rev ()) in
    (*wait for all our tasks*)
    let files_list = Promise.await files_list in
    (*now we can assign our results*)
    (* Vars.ui_state.jj_show $= log_res; *)
    Vars.ui_state.jj_branches $= branches;
    Vars.ui_state.jj_change_files $= files_list)
;;

let update_views_debouncer =
  Jj_tui.Debounce.make
    ~delay:0.05
    ~merge:( || )
    ~run:(fun cause_snapshot -> update_views ~cause_snapshot ())
    ()
;;

let update_views_async ?(cause_snapshot = false) () =
  Jj_tui.Debounce.push update_views_debouncer cause_snapshot
;;

(**Updates the status windows; Without snapshotting the working copy by default
   This should be called after any command that performs a change *)
let update_status ?(update_graph = true) ?(cause_snapshot = false) () =
  (* Keep graph redraw immediate, and refresh file/browser state asynchronously. *)
  if update_graph then Vars.ui_state.trigger_update $= ();
  update_views_async ~cause_snapshot ()
;;

let last_op_id = ref ""

(** Update all the jj views, if there has been some kind of change*)
let update_if_changed () =
  (* If the last op_id has changed then there has been a change somewhere and we should re-render. otherwise don't bother*)
  [%log info "Checking if there has been a change to the repo"];
  (* Wrap this just incase something fails so it shows an error *)
  safe_jj @@ fun () ->
  let this_op =
    jj_no_log
      ~color:false
      ~snapshot:true
      [ "op"; "log"; "--limit"; "1"; "-T"; "self.id()"; "--no-graph" ]
  in
  if !last_op_id <> this_op
  then (
    [%log info "updating ui state becasue of a change in the repo"];
    last_op_id := this_op;
    update_status ())
;;
