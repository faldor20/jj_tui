open Nottui
open Feather
open Lwd_infix
open Notty
open Eio.Std
module W = Nottui_widgets

module Vars = struct
  let quit = Lwd.var false

  (* let pool : Task_pool.t option ref = ref None *)

  (* let action : top_level_action option ref = ref None *)

  let eio_env : Eio_unix.Stdenv.base option ref = ref None

  (* let input_mode : input_mode Lwd.var = Lwd.var Navigate *)

  (* let init_ui_mode : ui_mode ref = ref Ui_multi_file *)

  (* let ui_mode : ui_mode Lwd.var = Lwd.var Ui_multi_file *)

  let term : Notty_unix.Term.t option ref = ref None
  let commit_message = Lwd.var ("", 0)
  let term_width_height : (int * int) Lwd.var = Lwd.var (0, 0)
end

(* let task_pool () = *)
(* Option.get !Vars.pool *)

let _eio_env () = Option.get !Vars.eio_env
let term () = Option.get !Vars.term

(** Makes a new process that has acess to all input and output
    This should be used for running other tui sub-programs *)
let switch_to_process env command =
  Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let stdout = Eio.Stdenv.stdout env in
  let stdin = Eio.Stdenv.stdin env in
  let stderr = Eio.Stdenv.stderr env in
  let proc = Eio.Process.spawn ~sw mgr ~stderr ~stdin ~stdout command in
  proc |> Eio.Process.await
;;

let term' : unit -> Notty_unix.Term.t = term

let full_term_sized_background =
  let$ term_width, term_height = Lwd.get Vars.term_width_height in
  Notty.I.void term_width term_height |> Nottui.Ui.atom
;;

let colored_string = Jj_tui.AnsiReverse.colored_string

(* Ui_loop.run (Lwd.pure (W.printf "Hello world"));; *)
let cmdArgs cmd args =
  let stdout, stderr = Feather.process cmd args |> Feather.collect stdout_and_stderr in
  stdout ^ stderr
;;

let jj args =
  print_endline "running command";
  let res = cmdArgs "jj" (List.concat [ args; [ "--color"; "always" ] ]) in
  if res |> String.length > 10000
  then String.sub res 0 10000 ^ "...truncated because it's really long"
  else res
;;

let vcount = Lwd.var I.empty

let _button =
  W.button (Printf.sprintf "run jj") (fun () ->
    vcount $= (cmdArgs "jj" [ "log"; "--color"; "always" ] |> colored_string))
  |> Lwd.pure
;;

(* let vQuit = Lwd.var false *)
let vExtern = Lwd.var `Nothing

let _quitButton =
  W.button (Printf.sprintf "quit ") (fun () -> Vars.quit $= true) |> Lwd.pure
;;

let ( <-$ ) f v = Lwd.map ~f (Lwd.get v)

(* let ( let<- ) v f = Lwd.map ~f (Lwd.get v) *)
let vShowStatus = Lwd.var I.empty
let vother = Lwd.var ""

let onChange () =
  let res = jj [ "show" ] |> colored_string in
  vShowStatus $= res;
  let res = jj []|>colored_string in
print_endline "finished commands";
  vcount $= res 
;;

let post_change state =
  onChange ();
  Lwd.set vExtern state
;;

let changeInputs key =
  let noOut args =
    let _ = jj args in
    `Handled
  in
  match key with
  | 'P' ->
    noOut [ "prev" ]
  | 'p' ->
    noOut [ "prev"; "--edit" ]
  | 'N' ->
    noOut [ "next" ]
  | 'n' ->
    noOut [ "next"; "--edit" ]
  | 'h' ->
    noOut [ "new" ]
  | 'c' ->
    post_change `Commit;
    `Handled
  | 'S' ->
    Lwd.set vExtern (`Cmd [ "jj"; "unsquash"; "-i" ]);
    `Handled
  | 's' ->
    Lwd.set vExtern (`Cmd [ "jj"; "squash"; "-i" ]);
    `Handled
  | 'R' ->
    Lwd.set vExtern (`Cmd [ "jj"; "resolve" ]);
    `Handled
  | _ ->
    `Unhandled
;;

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
        Vars.quit $= true;
        `Handled
      | `Key (`ASCII key, _) ->
        (match changeInputs key with
         | `Handled ->
           onChange ();
           `Handled
         | `Unhandled ->
           `Unhandled)
      | _ ->
        `Unhandled)
    ui
;;

(* let squashButton = *)
(* W.button "squash" (fun _ -> Lwd.set vExtern (`Cmd [ "jj"; "squash"; "-i" ])) *)
(* ;; *)

let mainUi env =
  let$* running = Lwd.get vExtern in
  match running with
  | `Cmd cmd ->
    (*We have this extra step to paint the terminal empty for one step*)
    Lwd.set vExtern @@ `RunCmd cmd;
    full_term_sized_background
  | `RunCmd cmd ->
    let exit_status_to_str y =
      match match y with `Exited x -> x | `Signaled x -> x with
      | 0 ->
        "success"
      | 1 ->
        "failure"
      | a ->
        Printf.sprintf "unknown code %d" a
    in
    let res = switch_to_process env cmd in
    let$ ui =
      W.vbox
        [
          W.string (Printf.sprintf "exit code:%s" (res |> exit_status_to_str)) |> Lwd.pure;
          W.button "back to main UI" (fun _ -> post_change `Nothing) |> Lwd.pure;
        ]
    in
    ui
    |> Ui.event_filter (fun event ->
      match event with
      | `Key (`ASCII ' ', _) ->
        post_change `Nothing;
        `Handled
      | _ ->
        `Unhandled)
  | (`Nothing | `Commit) as rest ->
    let$* pane =
      W.h_pane
        (Nottui_widgets.vbox
           [
             (* squashButton |> Lwd.pure; *)
             (* button; *)
             Ui.atom <-$ vcount (* quitButton *);
           ])
        (Ui.atom <-$ vShowStatus)
    in
    (match rest with
     | `Nothing ->
       inputs pane |> Lwd.pure
     | `Commit ->
       let exit () =
         post_change `Nothing;
         Lwd.set Vars.commit_message ("", 0)
       in
       let$ commit_field =
         W.zbox
           [
             W.string ~attr:A.(st underline) "                                       "
             |> Lwd.pure;
             W.edit_field
               (Lwd.get Vars.commit_message)
               ~on_change:(fun state -> Lwd.set Vars.commit_message state)
               ~on_submit:(fun (str, _) ->
                 let _ = jj [ "commit"; "-m"; str ] in
                 exit ());
           ]
       in
       Ui.zcat
         [
           pane;
           commit_field
           |> Widgets.border_box ~pad:Gravity.default ~label:"commit msg"
           |> Ui.resize ~pad:Widgets.neutral_grav;
         ]
       |> Ui.event_filter (fun event ->
         match event with
         | `Key (`Escape, _) ->
           exit ();
           `Handled
         | _ ->
           `Unhandled))
;;

let ui_loop ~quit ~term root =
  print_endline "starting loop";
  let renderer = Nottui.Renderer.make () in
  let root =
  let$ root = root in
  root
  |> Nottui.Ui.event_filter (fun x ->
  match x with
  | `Key (`Escape, []) ->
  Lwd.set quit true;
  `Handled
  | _ -> `Unhandled)
  in
  let rec loop () =
    if not (Lwd.peek quit)
    then (
      let term_width, term_height = Notty_unix.Term.size (term' ()) in
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
  onChange ();
  let term = Notty_unix.Term.create () in
  Vars.term := Some term;
  ui_loop ~quit:Vars.quit ~term (mainUi env)
;;

let start () = Eio_main.run @@ fun env -> Fiber.all [ (fun _ -> start_ui env) ];;

start ()
