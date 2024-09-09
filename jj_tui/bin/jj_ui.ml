open Picos_std_structured
open Notty
open Nottui
open Lwd_infix
open Global_funcs
open Jj_tui.Util
open Jj_tui
module Pio = Picos_io

module Ui = struct
  include Nottui.Ui

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
  module Jj_widgets = Jj_widgets.Make (Vars)
  module File_view = File_view.Make (Vars)
  module Graph_view = Graph_view.Make (Vars)
  module Show_view = Show_view.Make (Vars)

  let full_term_sized_background =
    let$ term_width, term_height = Lwd.get Vars.term_width_height in
    Notty.I.void term_width term_height |> Nottui.Ui.atom
  ;;

  let blue = I.string A.(fg blue ++ bg blue ++ st bold) "blue"

  let _quitButton =
    W.button (Printf.sprintf "quit ") (fun () -> Vars.quit $= true) |> Lwd.pure
  ;;

  let inputs ui =
    let$ input_state = Lwd.get ui_state.input
    and$ ui = ui in
    ui
    |> Ui.keyboard_area @@ fun event ->
       match event with
       | `ASCII 'q', _ ->
         Vars.quit $= true;
         `Handled
       | `Escape, _ ->
         (* TODO: I could refactor this and the rest of the mode handling so that when the popup is up and in focus it handles all key inputs *)
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

  (* shows a pretty box in the middle of the screen with our error in it*)
  let render_startup_error error =
    let message =
      match error with
      | `NotInRepo ->
        "Not in a jj repo."
      | `OtherError str ->
        str
      | `CantStartProcess e ->
        Printf.sprintf "Can't start jj process, maybe it's not installed?, error: %s" e
    in
    W.string message
    |> Lwd.pure
    |> W.Box.box
    |>$ Ui.resize ~sw:1 ~sh:1 ~mw:10000 ~mh:10000 ~crop:W.neutral_grav ~pad:W.neutral_grav
    |> inputs
  ;;

  (** The primary view for the UI with the file_view graph_view and summary*)
  let main_view () =
    let file_focus = Focus.make () in
    let graph_focus = Focus.make () in
    Focus.request graph_focus;
    let branch_focus = Focus.make () in
    let summary_focus = Focus.make () in
    W.hbox
      [
        (*left side window stack*)
        W.vbox
          [
            File_view.file_view file_focus
            |>$ Ui.resize ~w:5 ~sw:1 ~mw:1000
            |> W.Box.focusable ~focus:file_focus ~pad_h:0 ~pad_w:1
          ; Graph_view.graph_view ()
            |>$ Ui.resize ~sh:3 ~w:5 ~sw:1 ~mw:1000 ~h:10 ~mh:1000
            |> W.Box.focusable ~focus:graph_focus ~pad_h:0 ~pad_w:1
          ; W.Scroll.area (ui_state.jj_branches $-> Ui.atom)
            |> W.is_focused ~focus:branch_focus (fun ui focused ->
              ui
              |> Ui.keyboard_area (function
                | `ASCII k, [] ->
                  Jj_commands.handleInputs Jj_commands.default_list k
                | _ ->
                  `Unhandled)
              |> Ui.resize
                   ~w:5
                   ~sw:1
                   ~sh:(if focused then 3 else 0)
                   ~h:2
                   ~mh:1000
                   ~mw:1000)
            |> W.Box.focusable ~focus:branch_focus ~pad_h:0 ~pad_w:1
          ]
      ; (*Right side summary/status/fileinfo view*)
        Show_view.render ()
        |> W.Scroll.area
        (* let mw=Int.max (Ui.layout_max_width ui) 100 in *)
        |>$ Ui.resize ~w:0 ~sh:3 ~sw:2 ~mw:10000 ~mh:10000
        |> W.on_focus ~focus:summary_focus (Ui.resize ~sw:3 ~mw:1000)
        |> W.Box.focusable ~focus:summary_focus ~pad_h:0 ~pad_w:1
      ]
    (*These outer prompts can popup and show them selves over the main view*)
    |> W.Overlay.text_prompt ~char_count:true ~show_prompt_var:ui_state.show_prompt
    |> W.Overlay.popup ~show_popup_var:ui_state.show_popup
    |> W.Overlay.selection_list_prompt_filterable
         ~show_prompt_var:ui_state.show_string_selection_prompt
    |> inputs
  ;;

  (** Shows the op log *)
  let log_view () =
    jj_no_log [ "op"; "log"; "--limit"; "200" ]
    |> AnsiReverse.colored_string
    |> Ui.atom
    |> Ui.resize ~mh:1000 ~mw:10000
    |> Lwd.pure
    |> W.Scroll.area
    |> W.Box.box ~pad_w:1 ~pad_h:0
    |> inputs
  ;;

  let mainUi () =
    (*we want to initialize our states and keep them up to date*)
    match check_startup () with
    | `Good ->
      update_status ~cause_snapshot:true ();
      Flock.fork (fun () ->
        while true do
          Picos.Fiber.sleep ~seconds:5.0;
          (*we need to lock this becasue we could end up updating while the ui is rendering*)
          (* Vars.render_mutex |> Eio.Mutex.lock; *)
          update_status ~cause_snapshot:true ()
          (* Vars.render_mutex |> Eio.Mutex.unlock *)
        done;
        ());
      let$* running = Lwd.get ui_state.view in
      (match running with
       | `Cmd_I cmd ->
         (*We have this extra step to paint the terminal empty for one step*)
         Lwd.set ui_state.view @@ `RunCmd cmd;
         full_term_sized_background
       | `RunCmd cmd ->
         Jj_widgets.interactive_process ("jj" :: cmd)
       | `Main ->
         W.keyboard_tabs [ ("Main", fun _ -> main_view ()); "Op log", log_view ])
    | (`CantStartProcess _ | `NotInRepo | `OtherError _) as other ->
      render_startup_error other
  ;;
end
