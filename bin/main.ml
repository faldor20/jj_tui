open Nottui;;
open Feather;;
open Lwd_infix;;
module W = Nottui_widgets;;


(* Ui_loop.run (Lwd.pure (W.printf "Hello world"));; *)

let cmdArgs cmd args=
  let (stdout,stderr)=Feather.process cmd args|>Feather.collect stdout_and_stderr in
  stdout^stderr
;;
let cmd cmd =
  let (stdout,stderr)=Feather.process cmd []|>Feather.collect stdout_and_stderr in
  stdout^stderr

;;


let vcount = Lwd.var "";;

let button = 
  W.button (Printf.sprintf "run jj" )
           (fun () ->  vcount $= (cmd"jj"))
  |>Lwd.pure
         ;;
let vQuit=Lwd.var false;;

let quitButton  = 
  W.button (Printf.sprintf "quit " )
           (fun () ->  vQuit $= true)
  |>Lwd.pure
         ;;


let (<-$) f v=Lwd.map ~f (Lwd.get v);; 
let vShowStatus= Lwd.var "";;
let inputs ui=
  Ui.event_filter (fun event->
    match event with
    |`Key (`ASCII 'l',_)-> 
        let res=cmd "jj"in
        vcount$=res;

        `Handled
    |`Key (`ASCII 's',_)-> 
        let res=cmdArgs "jj" ["show"]in
        vShowStatus$=res;

        `Handled
    |`Key (`ASCII 'p',_)-> 
        let _=cmdArgs "jj"["prev"]in

        `Handled
    |`Key (`ASCII 'n',_)-> 
        let _=cmdArgs "jj "["next"]in

        `Handled
    |_->`Unhandled
      ) ui
  ;;
let mainUi= 
  Lwd.map ~f:inputs @@
  W.h_pane
  (Nottui_widgets.vbox [
    button;
    W.string <-$ vcount;
    quitButton])
  (W.string <-$ vShowStatus)
;;
      

Ui_loop.run ~quit:vQuit (mainUi);;

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
