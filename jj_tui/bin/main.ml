open Lwd_infix
module Vars = Global_vars.Vars
open Nottui
module Jj_ui = Jj_ui.Make (Vars)
open Picos_std_structured

let ui_loop ~quit ~term root =
  print_endline "starting loop";
  let renderer = Nottui.Renderer.make () in
  let root =
    let$ root = root in
    root
    |> Nottui.Ui.event_filter (fun x ->
      match x with
      | `Key (`ASCII 'q', [`Ctrl]) ->
        Lwd.set quit true;
        `Handled
      | _ ->
        `Unhandled)
  in
  let rec loop () =
    if not (Lwd.peek quit)
    then (
      let start_time = Sys.time () in
      let term_width, term_height = Notty_unix.Term.size (Vars.get_term ()) in
      let prev_term_width, prev_term_height = Lwd.peek Vars.term_width_height in
      if term_width <> prev_term_width || term_height <> prev_term_height
      then Lwd.set Vars.term_width_height (term_width, term_height);
      (* Vars.render_mutex |> Eio.Mutex.lock; *)
      Nottui.Ui_loop.step
        ~process_event:true
        ~timeout:0.01
        ~renderer
        term
        (Lwd.observe @@ root);

      (*Sleep for a bit to stop spinning the cpu
      TODO: May not be needed, nottui may sleep for a bit anyway
      *)
      let end_time = Sys.time () in
      let elapsed = end_time -. start_time in
      let sleep_time = max 0.01 (0.01 -. elapsed) in
      Picos_io.Unix.sleepf sleep_time;
      loop ())
  in
  loop ()
;;

(*TODO:For hosting a subprocess i should look into using EIO and Ui_loop.step like some of the other libraries built with nottui*)
let start_ui () =
  (*initialse the state*)
  let term = Notty_unix.Term.create () in
  Vars.term := Some term;
  ui_loop ~quit:Vars.quit ~term (Jj_ui.mainUi ());
  Flock.terminate ()

;;

let start () =
  Picos_mux_multififo.run_on ~n_domains:8 (fun _ ->
    Flock.join_after @@ fun () -> start_ui ())
(* Picos_mux_multififo.run (fun () -> Flock.join_after (fun _ -> start_ui ())) *)
;;

start ()
