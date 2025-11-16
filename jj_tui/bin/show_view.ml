open Picos_std_sync
open Picos_std_structured
open Jj_tui.Logging
module Log = (val src_log ~src:(Logs.Src.create "status view"))

type detail_state =
  | Loading
  | Loaded of Notty.image
  | Failed

type status_state =
  | File_preview of (string * string) (*revision,filepath*)
  | Graph_preview of string (*revision*)
[@@deriving show]

let statusStream = Stream.create ()
let lastMessage = ref None

let push_status status =
  lastMessage := Some status;
  Stream.push statusStream status
;;

(** pushes the last message to the queue again to re-render everything *)
let re_render () = !lastMessage |> Option.iter push_status

module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Nottui
  open! Jj_tui.Util
  open Global_vars
  open Jj_tui

  type view_state = {
      summary : Notty.image
    ; detail : detail_state
  }

  let viewState = Lwd.var { summary = I.empty; detail = Loading }

  let render_summary = function
    | File_preview (rev, file) ->
      let command =
        if file != ""
        then [ "diff"; "--summary"; "-r"; rev; file ]
        else [ "show"; "--summary"; "-r"; rev ]
      in
      let log = jj_no_log command in
      Control.yield ();
      let res = log |> AnsiReverse.colored_string in
      Control.yield ();
      res
    | Graph_preview rev ->
      let log = jj_no_log ~snapshot:false [ "show"; "--summary"; "-r"; rev ] in
      Control.yield ();
      let res = log |> AnsiReverse.colored_string in
      Control.yield ();
      res
  ;;

  let render_detail = function
    | File_preview (rev, file) ->
      let command =
        if file != "" then [ "diff"; "-r"; rev; file ] else [ "show"; "-r"; rev ]
      in
      let log = jj_no_log command in
      Control.yield ();
      let res = log |> AnsiReverse.colored_string in
      Control.yield ();
      res
    | Graph_preview rev ->
      let log = jj_no_log ~snapshot:false [ "diff"; "-r"; rev ] in
      Control.yield ();
      let res = log |> AnsiReverse.colored_string in
      Control.yield ();
      res
  ;;

  let get_latest_message cursor =

      let rec seek_latest last cursor=
        let peeked=Stream.peek_opt cursor in
        match peeked with
        |Some (last,new_cursor)->
          seek_latest last new_cursor
        |None->
          [%log debug "skipping to next status because two were queued"];
          (last,cursor)
      in
      let msg, new_cursor = cursor |> Stream.read in

      (*little 50ms delay to let us move to the next one if it's ready*)
      Picos.Fiber.sleep ~seconds:0.05;
      (*if the queue isn't empty just skip the current because we really only ever want the newest*)
      seek_latest msg new_cursor


  (* Wait for messages to come in the stream.
     When a message comes, we try to render it.
     If a new message comes, we cancel the current computation and then start the new rendering
  *)
  let render_loop stream =
    let current_summary_computation = ref (Promise.of_value ()) in
    let current_detail_computation = ref (Promise.of_value ()) in
    let current_loading_computation = ref (Promise.of_value ()) in
    let cursor = ref (Stream.tap stream) in
    while true do
      [%log debug "waiting for next status"];
      let msg,new_cursor=get_latest_message  !cursor in
      cursor:=new_cursor;
      [%log debug "cancelling older status because of new message"];
      Promise.terminate_after ~seconds:0. !current_summary_computation;
      Promise.terminate_after ~seconds:0. !current_detail_computation;
      Promise.terminate_after ~seconds:0. !current_loading_computation;
      current_summary_computation
      := Flock.fork_as_promise (fun () ->
           try
             [%log debug "Rendering status summary with: %a" pp_status_state msg];
             let summary = render_summary msg in
             viewState $= { (Lwd.peek viewState) with summary }
           with
           | _ ->
             [%log
               warn
                 "summary render failed. If this happens once it's probably just because \
                  a node was deleted. If it keeps happening and the user can't see \
                  anything, obviously this is important"]);
      current_loading_computation
      := Flock.fork_as_promise (fun () ->
           (* If it's been more than half a second, show the state as loading*)
           Control.sleep ~seconds:0.3;
           viewState $= { (Lwd.peek viewState) with detail = Loading });
      current_detail_computation
      := Flock.fork_as_promise (fun () ->
           try
             [%log debug "Rendering status detail with: %a" pp_status_state msg];
             let detail = Loaded (render_detail msg) in
             (*Make sure the loading is done*)
             !current_loading_computation |> Promise.terminate;
             viewState $= { (Lwd.peek viewState) with detail }
           with
           | _ ->
             [%log
               warn
                 "detail render failed. If this happens once it's probably just because \
                  a node was deleted. If it keeps happening and the user can't see \
                  anything, obviously this is important"];
             viewState $= { (Lwd.peek viewState) with detail = Failed })
    done
  ;;

  let render focus =
    Flock.fork (fun () -> render_loop statusStream);
    Lwd.get viewState |>$ fun { summary; detail } ->
    let detail_view =
      match detail with
      | Loading ->
        I.string A.empty "Loading..."
      | Loaded image ->
        image
      | Failed ->
        I.string A.empty "Failed to load diff"
    in
    I.vcat [ summary; I.void 0 1; detail_view ]
    |> Ui.atom
    |> Ui.keyboard_area (function
      | `Escape, [] ->
        Focus.release_reversable focus;
        `Handled
      | _ ->
        `Unhandled)
  ;;

  (*
     - recieve events
     - compute the value for the status view
     - update the view
     - finish
  *)

  (*
     - Render whatever the latest state is
     - Needs to be cancellable, if a new state appears, render that new state
     - ideally idependant
  *)
end
