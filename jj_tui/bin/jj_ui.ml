open Picos_std_structured
open Notty
open Nottui
open Lwd_infix
open Global_funcs
open Jj_tui.Util
open Jj_tui
open Logging
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

  let rec forward_events handlers event =
    match handlers with
    | h :: rest ->
      (match h event with `Unhandled -> forward_events rest event | other -> other)
    | [] ->
      `Unhandled
  ;;

  let inputs ?(custom = fun _ -> `Unhandled) ui =
    ui
    |>$ Ui.keyboard_area @@ fun event ->
        event
        |> forward_events
             [
               custom
             ; (function
                 | `ASCII 'q', _ ->
                   Vars.quit $= true;
                   `Handled
                 (* | `Arrow _, [ `Ctrl ] *)
                 (* | `Arrow _, [ `Meta ] *)
                 | `Tab, [] ->
                   `Handled
                 | `Tab, [ `Meta ] | `Tab, [ `Meta; `Shift ] ->
                   `Handled
                 | _ ->
                   `Unhandled)
             ; (fun event ->
                 match
                   ( Lwd.peek ui_state.input
                   , Lwd.peek ui_state.show_prompt
                   , Lwd.peek ui_state.show_popup )
                 with
                 | `Mode _, _, _ ->
                   (match event with
                    | `Escape, [] ->
                      show_popup None;
                      ui_state.input $= `Normal;
                      `Handled
                    | _ ->
                      `Unhandled)
                 | `Normal, None, None ->
                   (* only control focus when no popup and no input mode is active*)
                   (match event with
                    | `Arrow `Left, _ ->
                      `Remap (`Focus `Up, [])
                    | `Arrow `Right, _ ->
                      `Remap (`Focus `Down, [])
                    | _ ->
                      `Unhandled)
                 | _ ->
                   `Unhandled)
             ]
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

  (** Makes a UI element responsive to terminal width and focus state 
      - When focused: shows at full width if terminal is wide enough, or fills terminal if narrow
      - When unfocused: shows at normal width if terminal is wide enough, or collapses if narrow *)
  let responsive_view  ?(shrunk_width=0) ?(shrink_on= `Focus) ~focus ui =
    let$* w, h = Lwd.get Vars.term_width_height in 
  let$ ui = ui 
and$ focus = focus|>Focus.status in

    let should_shrink = match shrink_on with
      | `Focus -> focus|>Focus.has_focus
      | `Unfocus -> not (focus|>Focus.has_focus)

    in
    let threhold=(Lwd.peek Vars.config).single_pane_width_threshold in
    if should_shrink
    then if w < threhold 
      then ui |> Ui.resize ~w:w ~mw:w
      else ui
    else if w < threhold
      then ui |> Ui.resize ~w:shrunk_width ~mw:shrunk_width
      else ui
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
            File_view.file_view ~focus:file_focus summary_focus
            (* |>$ Ui.resize ~w:5 ~sw:1 ~mw:1000 *)
            |> W.is_focused ~focus:file_focus (fun ui focused ->
              ui
              |> Ui.resize
                   ~w:5
                   ~sw:1
                   ~sh:12
                   ~h:2
                     (*Lets our box get bigger when focused but not take up unecissarry spaec*)
                   ~mh:(if focused then Int.min (ui |> Ui.layout_max_height) 12 else 2)
                   ~mw:1000)
            |> W.Box.focusable ~focus:file_focus ~pad_h:0 ~pad_w:1
          ; Graph_view.graph_view ~focus:graph_focus summary_focus ()
            |>$ Ui.resize ~sh:3 ~w:5 ~sw:1 ~mw:1000 ~h:10 ~mh:1000
            |> W.Box.focusable ~focus:graph_focus ~pad_h:0 ~pad_w:1
          ; W.Scroll.v_area (ui_state.jj_branches $-> Ui.atom)
            |> W.is_focused ~focus:branch_focus (fun ui focused ->
              ui
              |> Ui.keyboard_area (fun k ->
                Jj_commands.handleInputs Jj_commands.default_list k)
              |> Ui.resize
                   ~w:5
                   ~sw:1
                   ~sh:(if focused then 6 else 0)
                   ~h:2
                   ~mh:1000
                   ~mw:1000)
            |> W.Box.focusable ~focus:branch_focus ~pad_h:0 ~pad_w:1
          ]
          |> responsive_view  ~focus:summary_focus ~shrink_on:`Unfocus  ~shrunk_width:0 
      ; (*Right side summary/status/fileinfo view*)
        (let ui =
           Show_view.render summary_focus
           |> W.Scroll.area
           (* let mw=Int.max (Ui.layout_max_width ui) 100 in *)
           |>$ Ui.resize ~w:3 ~sh:3 ~sw:1 ~mw:10000 ~mh:10000
           |> W.on_focus ~focus:summary_focus (Ui.resize ~sw:3 ~mw:1000)
           |> W.Box.focusable ~focus:summary_focus ~pad_h:0 ~pad_w:1
         in
         responsive_view ~focus:summary_focus ~shrunk_width:0  ui)
      ]
    (*These outer prompts can popup and show them selves over the main view*)
    |> W.Overlay.text_prompt ~char_count:true ~show_prompt_var:ui_state.show_prompt
    |> W.Overlay.popup ~show_popup_var:ui_state.show_popup
    |> W.Overlay.selection_list_prompt_filterable
         ~list_outline_focus_attr:A.(empty) (*highlighting the outline inside the propt is a bit over the top*)
         ~show_prompt_var:ui_state.show_string_selection_prompt
    |> inputs ~custom:(fun x -> Jj_commands.handleInputs Jj_commands.default_list x)
  ;;

  (* block all normal focus keys *)

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
    (* first lets load the config*)
    Vars.config $= Config.load_config ();
    [%log info "loaded config"];
    (*we want to initialize our states and keep them up to date*)
    let$* startup_result = check_startup () in
    match startup_result with
    | `Good ->
      Flock.fork (fun () ->
        update_if_changed ();
        while true do
          Picos.Fiber.sleep ~seconds:5.0;
          update_if_changed ()
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
