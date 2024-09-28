open Picos_std_sync
open Picos_std_structured
open Jj_tui.Logging

type status_state =
  | File_preview of (string * string) (*revision,filepath*)
  | Graph_preview of string (*revision*)
[@@deriving show]

let statusStream = Stream.create ()
let lastMessage = None
let pushStatus status = Stream.push statusStream status

(** pushes the last message to the queue again to re-render everything *)
let reRender () = lastMessage |> Option.iter pushStatus

module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Nottui
  open! Jj_tui.Util
  open Global_vars
  open Jj_tui

  let viewState = Lwd.var I.empty

  let render_file_preview (rev, file) =
    (* we yield a bunch here to provide places for the computation to be terminated*)
    if file != ""
    then (
      let log = jj_no_log [ "diff"; "-r"; rev; file ] in
      Control.yield ();
      let res = log |> AnsiReverse.colored_string in
      Control.yield ();
      res)
    else I.string A.empty ""
  ;;

  let render_graph_preview rev =
    let log = jj_no_log ~snapshot:false [ "show"; "-s"; "--color-words"; "-r"; rev ] in
    Control.yield ();
    let res = log |> AnsiReverse.colored_string in
    Control.yield ();
    res
  ;;

  (* Wait for messages to come in the stream.
     When a message comes, we try to render it.
     If a new message comes, we cancel the current computation and then start the new rendering
  *)
  let render_loop stream =
    let current_computation = ref (Promise.of_value ()) in
    let cursor = ref (Stream.tap stream) in
    while true do
      let msg, new_cursor = !cursor |> Stream.read in
      cursor := new_cursor;
      Promise.terminate_after ~seconds:0. !current_computation;
      current_computation
      := Flock.fork_as_promise (fun () ->
           [%log debug "Rendering status view with: %a" pp_status_state msg];
           viewState
           $=
           match msg with
           | File_preview (rev, file) ->
             render_file_preview (rev, file)
           | Graph_preview rev ->
             render_graph_preview rev)
    done
  ;;

  let render focus =
    Flock.fork (fun () -> render_loop statusStream);
    Lwd.get viewState |>$ fun x ->
    x
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
