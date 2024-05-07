open Nottui
open Feather
open Lwd_infix
open Notty
module W = Nottui_widgets

let colored_string = Jj_tui.AnsiReverse.colored_string

(* Ui_loop.run (Lwd.pure (W.printf "Hello world"));; *)
let cmdArgs cmd args =
  let stdout, stderr =
    Feather.process cmd args |> Feather.collect stdout_and_stderr
  in
  stdout ^ stderr

let jj args = cmdArgs "jj" (List.concat [ args; [ "--color"; "always" ] ])
let vcount = Lwd.var I.empty

let _button =
  W.button (Printf.sprintf "run jj") (fun () ->
      vcount $= (cmdArgs "jj" [ "log"; "--color"; "always" ] |> colored_string))
  |> Lwd.pure

let vQuit = Lwd.var false

let _quitButton =
  W.button (Printf.sprintf "quit ") (fun () -> vQuit $= true) |> Lwd.pure

let ( <-$ ) f v = Lwd.map ~f (Lwd.get v)

(* let ( let<- ) v f = Lwd.map ~f (Lwd.get v) *)
let vShowStatus = Lwd.var I.empty
let vother = Lwd.var ""

let onChange () =
  let res = jj [ "show" ] |> colored_string in
  vShowStatus $= res;
  let res = jj [] in
  vcount $= colored_string res

let changeInputs key =
  let noOut args =
    let _ = jj args in
    `Handled
  in
  match key with
  | 'P' -> noOut [ "prev" ]
  | 'p' -> noOut [ "prev"; "--edit" ]
  | 'N' -> noOut [ "next" ]
  | 'n' -> noOut [ "next"; "--edit" ]
  | 'S' -> noOut [ "unsquash"; "-i"; "--tool"; "sublime_merge" ]
  | _ -> `Unhandled

let inputs ui =
  Ui.event_filter
    (fun event ->
      match event with
      | `Key (`ASCII 's', _) ->
          let res = jj [ "show" ] in
          vShowStatus $= (res |> colored_string);

          `Handled
      | `Key (`ASCII 'l', _) ->
          let res = jj [] in
          vcount $= colored_string res;
          vother $= res;

          `Handled
      | `Key (`ASCII 'q', _) ->
          vQuit $= true;

          `Handled
      | `Key (`ASCII key, _) -> (
          match changeInputs key with
          | `Handled ->
              onChange ();
              `Handled
          | `Unhandled -> `Unhandled)
      | _ -> `Unhandled)
    ui

let mainUi =
  let$ pane =
    W.h_pane
      (Nottui_widgets.vbox
         [ (* button;  *) Ui.atom <-$ vcount (* quitButton  *) ])
      (Ui.atom <-$ vShowStatus)
  in
  inputs pane
;;

(*TODO:For hosting a subprocess i should look into using EIO and Ui_loop.step like some of the other libraries built with nottui*)
Ui_loop.run ~quit:vQuit mainUi
(* let my_image=(Notty.I.string Notty.A.empty "\027[32mThis is in green %s\027[0m" ) in *)
(* let my_image =
     Jj_tui.AnsiReverse.Cap.parse_ansi_escape_codes
       "\027[32mThis is in green %s\027[0m "
     |> List.map (fun (x, str) -> Notty.I.string x str)
   ;;

   Notty_unix.output_image (my_image |> List.hd) *)

(*
type tree = Tree of string * (unit -> tree list)

let rec tree_ui (Tree (label, child)) =
  let opened = Lwd.var false in
  let render is_opened =
    let btn_text = if is_opened then "[-] " else "[+] " in
    let btn_action () = Lwd.set opened (not is_opened) in
    let btn = W.button (btn_text ^ label) btn_action in
    let layout node forest =
      Ui.join_y node (Ui.join_x (Ui.space 2 0) forest) 
    in
    if is_opened 
    then Lwd.map ~f:(layout btn) (forest_ui (child ()))
    else Lwd.pure btn
  in
  Lwd.join (Lwd.map ~f:render (Lwd.get opened))
  
and forest_ui nodes = 
  Lwd_utils.pack Ui.pack_y 
    (List.map tree_ui nodes)
;;

let rec fake_fs () = [
  Tree ("bin", fake_fs);
  Tree ("home", fake_fs);
  Tree ("usr", fake_fs);
] in

Ui_loop.run (forest_ui (fake_fs ()));;
*)
