module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  module W = Nottui_widgets
  open Nottui
  open! Jj_tui.Util
  module Wd = Jj_tui.Widgets
  open Jj_commands.Make (Vars)

  let selected_file = Lwd.var ""

  let rec command_mapping =
    [
      {
        key = 'h'
      ; description = "Show help"
      ; cmd =
          Fun
            (fun _ ->
              ui_state.show_popup $= Some (commands_list_ui command_mapping, "Help");
              ui_state.input $= `Mode (fun _ -> `Unhandled))
      }
    ; {
        key = 'm'
      ; description = "Move file to other commit"
      ; cmd =
          PromptThen
            ( "Revision to move file to"
            , fun rev ->
                Cmd
                  [ "squash"; "-u"; "--from"; Lwd.peek ui_state.selected_revision; "--into"; rev; Lwd.peek selected_file ]
            )
      }
    ; {
        key = 'd'
      ; description = "Restore to previous revision (git discard)"
      ; cmd =
          Dynamic
            (fun _ ->
              let selected = Lwd.peek selected_file in
              let rev=Lwd.peek Vars.ui_state.selected_revision in
              confirm_prompt
                ("discard all changes to '" ^ selected ^ "' in rev "^ rev)
                (Cmd [ "restore";"--to";rev;"--from";rev^"-"; selected ]))
      }
    ]
  ;;

  let file_view sw () =
    let file_uis =
      let$ files = Lwd.get Vars.ui_state.jj_change_files in
      files
      |> List.map (fun (_modifier, file) ->
        Wd.{ data = file; ui = Wd.selectable_item (W.string file) })
    in
    Wd.selection_list_custom
      ~on_selection_change:(fun x ->
        Eio.Fiber.fork ~sw @@ fun _ ->
        Vars.update_ui_state @@ fun _ -> 
        Lwd.set selected_file x;
        )
      ~custom_handler:(fun _ _ key ->
        match key with `ASCII k, [] -> handleInputs command_mapping k | _ -> `Unhandled)
      file_uis
  ;;

  (**Get the status for the currently selected file*)
  let file_status () =
    let$ selected = Lwd.get selected_file 
    and$rev = Lwd.get Vars.ui_state.selected_revision in
    if selected != "" then jj_no_log [ "diff";"-r";rev ; selected ] else ""
  ;;
end
