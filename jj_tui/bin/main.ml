open Lwd_infix
module Vars = Global_vars.Vars
open Nottui
module Jj_ui = Jj_ui.Make (Vars)
open Picos_std_structured
open Jj_tui.Logging

let await_read_unix fd timeout : [ `Ready | `NotReady ] =
  let rec select () =
    match Unix.select [ fd ] [] [ fd ] timeout with
    | [], [], [] ->
      `NotReady
    | _ ->
      `Ready
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      select ()
  in
  select ()
;;

(* let file_logger ~logs_stream=
   let logs_crs=Picos_std_sync.Stream.tap logs_stream in
   let file=Picos_io.Unix.openfile "" in
   let handle_log cursor=
   let log,cursor =Picos_std_sync.Stream.read cursor in

   Picos_io.Unix.wri
*)

let ui_loop ~quit ~term root =
  let renderer = Nottui.Renderer.make () in
  let root =
    let$ root = root in
    root
    |> Nottui.Ui.event_filter (fun x ->
      match x with
      | `Key (`ASCII 'q', [ `Ctrl ]) ->
        Lwd.set quit true;
        `Handled
      | _ ->
        `Unhandled)
  in
  let tick () =
    let term_width, term_height = Notty_unix.Term.size (Vars.get_term ()) in
    let prev_term_width, prev_term_height = Lwd.peek Vars.term_width_height in
    if term_width <> prev_term_width || term_height <> prev_term_height
    then Lwd.set Vars.term_width_height (term_width, term_height)
  in
  Nottui_picos.Ui_loop.run   ~tick ~term ~renderer ~quit root
;;

let start_ui () =
  (*initialse the state*)
  let term = Notty_unix.Term.create ~mouse:false () in
  Vars.term := Some term;
  ui_loop ~quit:Vars.quit ~term (Jj_ui.mainUi ());
  Flock.terminate ()
;;

let start () =
  Picos_mux_multififo.run_on ~n_domains:4 (fun _ ->
    Flock.join_after @@ fun () ->
    init_logging ();
    start_ui ())
(* Picos_mux_multififo.run (fun () -> Flock.join_after (fun _ -> start_ui ())) *)
;;

start ()
