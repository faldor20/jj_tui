open Eio.Std
open Lwd_infix
module W = Nottui_widgets
module Vars = Global_vars.Vars
module Jj_ui = Jj_ui.Make (Vars)

let ui_loop ~quit ~term root =
  print_endline "starting loop";
  let renderer = Nottui.Renderer.make () in
  let root =
    let$ root = root in
    root
    |> Nottui.Ui.event_filter (fun x ->
      match x with
      | `Key (`Delete, []) ->
        Lwd.set quit true;
        `Handled
      | _ ->
        `Unhandled)
  in
  let rec loop () =
    if not (Lwd.peek quit)
    then (
      let term_width, term_height = Notty_unix.Term.size (Vars.get_term ()) in
      let prev_term_width, prev_term_height = Lwd.peek Vars.term_width_height in
      if term_width <> prev_term_width || term_height <> prev_term_height
      then Lwd.set Vars.term_width_height (term_width, term_height);
      Nottui.Ui_loop.step
        ~process_event:true
        ~timeout:0.05
        ~renderer
        term
        (Lwd.observe @@ root);
      Eio.Fiber.yield ();
      loop ())
  in
  loop ()
;;

(*TODO:For hosting a subprocess i should look into using EIO and Ui_loop.step like some of the other libraries built with nottui*)
let start_ui env =
  (*initialse the state*)
  let term = Notty_unix.Term.create () in
  Vars.term := Some term;
  Vars.eio_env := Some env;
  ui_loop ~quit:Vars.quit ~term (Jj_ui.mainUi env)
;;


let start () = Eio_main.run @@ fun env -> Fiber.all [ (fun _ -> start_ui env) ];;

start ()
