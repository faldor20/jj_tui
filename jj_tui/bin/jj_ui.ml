open Notty
open Nottui
open Lwd_infix
open Global_funcs
open Jj_tui.Util
open Jj_tui
module W = Nottui_widgets
module Wd = Widgets

module Ui = struct
  include Nottui.Ui

  (* let pad ?(l=0) ?(r=0) ?(t=0) ?(b=0) ui= *)
  (* match l,r,t,b with *)
  (* |(0,0,_,_)-> *)
  (* |(_,_,0,0)-> *)
  (* |(_,_,_,_)-> *)
  let pad v h ui =
    let o_w = Ui.layout_width ui in
    let o_h = Ui.layout_height ui in
    Ui.resize ~w:(o_w + h) ~h:(o_h + v) ui
  ;;
end

module Make (Vars : Global_vars.Vars) = struct
  open Jj_process.Make (Vars)
  open Vars
  module Jj_commands = Jj_commands.Make (Vars)

  (* let task_pool () = *)
  (* Option.get !Vars.pool *)

  let full_term_sized_background =
    let$ term_width, term_height = Lwd.get Vars.term_width_height in
    Notty.I.void term_width term_height |> Nottui.Ui.atom
  ;;

  (* let vQuit = Lwd.var false *)

  let _quitButton =
    W.button (Printf.sprintf "quit ") (fun () -> Vars.quit $= true) |> Lwd.pure
  ;;

  (* let ( let<- ) v f = Lwd.map ~f (Lwd.get v) *)

  let post_change new_view =
    on_change ();
    Lwd.set ui_state.view new_view
  ;;

  let inputs ui =
    let$ input_state = Lwd.get ui_state.input in
    let handler =
      match input_state with
      | `Normal ->
        Jj_commands.command_input Jj_commands.commandMapping
      | `Mode handle ->
        handle
    in
    ui
    |> Ui.keyboard_area @@ fun event ->
       match event with
       | `ASCII 'q', _ ->
         Vars.quit $= true;
         `Handled
       | `ASCII key, _ ->
         (match handler key with
          | `Handled ->
            on_change ();
            `Handled
          | `Unhandled ->
            `Unhandled)
       | `Escape, _ ->
         (*TODO: I could refactor this and the rest of the mode handling so that when the popup is up and in focus it handles all key inputs *)
         (match input_state with
          | `Mode _ ->
            ui_state.show_popup $= None;
            ui_state.input $= `Normal;
            `Handled
          | _ ->
            `Unhandled)
       | _ ->
         `Unhandled
  ;;

  (** Start a process that will take full control of both stdin and stdout.
      This is used for interactive diffs and such*)
  let interactive_process env cmd =
    let exit_status_to_str y =
      match match y with `Exited x -> x | `Signaled x -> x with
      | 0 ->
        "success"
      | 1 ->
        "failure%s"
      | a ->
        Printf.sprintf "unknown code %d" a
    in
    let res = switch_to_process env cmd in
    let$ ui =
      W.vbox
        [
          W.string (Printf.sprintf "exit code:%s" (res |> exit_status_to_str)) |> Lwd.pure;
          W.button "back to main UI" (fun _ -> post_change `Main) |> Lwd.pure;
        ]
    in
    ui
    |> Ui.keyboard_area (fun event ->
      match event with
      | `ASCII ' ', _ ->
        post_change `Main;
        `Handled
      | _ ->
        `Unhandled)
  ;;

  let renderSizeMonitor ui =
    let size = Lwd.var (0, 0) in
    W.vbox
      [
        ui
        |>$ Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then size $= (w, h));
        (let$ size = Lwd.get size in
         I.strf "w:%d h:%d" (fst size) (snd size) |> Ui.atom);
      ]
  ;;

  (* let squashButton = *)
  (* W.button "squash" (fun _ -> Lwd.set ui_state.view (`Cmd [ "jj"; "squash"; "-i" ])) *)
  (* ;; *)

  let mainUi env =
    (*we want to initialize our states*)
    on_change ();
    let$* running = Lwd.get ui_state.view in
    match running with
    | `Cmd_I cmd ->
      (*We have this extra step to paint the terminal empty for one step*)
      Lwd.set ui_state.view @@ `RunCmd cmd;
      full_term_sized_background
    | `RunCmd cmd ->
      interactive_process env ("jj" :: cmd)
    | `Main ->
      let v_cmd_out = Lwd.var "" in
      let$* pane =
        W.h_pane
          (W.vbox
             [
               Wd.v_scroll_area (ui_state.jj_tree $-> (I.pad ~l:1 ~r:1 >> Ui.atom))
               |>$ Ui.resize ~sh:3|>renderSizeMonitor;
               Widgets.h_rule |> Lwd.pure;
               Wd.v_scroll_area  (ui_state.jj_branches $-> Ui.atom) |>$ Ui.resize ~sh:1;
               Widgets.h_rule |> Lwd.pure;
               Widgets.h_rule |> Lwd.pure;
               Wd.v_scroll_area 
                 (ui_state.command_log
                  |> Lwd.get
                  |> Lwd.bind ~f:(List.map (W.string >> Lwd.pure) >> W.vlist))
               |>$ Ui.resize ~sh:1;
               v_cmd_out $-> W.string;
             ])
          (Wd.v_scroll_area 
             ((fun x -> x |> I.pad ~l:1 ~r:1 |> Ui.atom) <-$ ui_state.jj_show))
        |> Widgets.general_prompt ~char_count:true ~show_prompt_var:ui_state.show_prompt
        |> Widgets.popup ~show_popup_var:ui_state.show_popup
      in
      pane |> inputs
  ;;
end
