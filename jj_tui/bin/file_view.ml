module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Jj_tui
  module W = Nottui_widgets
  open Nottui
  open! Jj_tui.Util
  module Wd = Widgets
  open Jj_commands.Make (Vars)

  let selected_file = Lwd.var ""

  let command_mapping =
    [
      {
        key = 'm'
      ; description = "Move commit"
      ; cmd =
          PromptThen
            ( "Revision to move file to"
            , fun rev ->
                Cmd
                  [ "squash"; "-u"; "--from"; "@"; "--into"; rev; Lwd.peek selected_file ]
            )
      }
    ; {
        key = 'd'
      ; description = "Restore to previous revision (git discard)"
      ; cmd =
          Dynamic
            (fun _ ->
              let selected = Lwd.peek selected_file in
              confirm_prompt
                ("discard all changes to '" ^ selected ^ "' in this revision")
                (Cmd [ "restore"; selected ]))
      }
    ]
  ;;

  let file_view () =
    let file_uis =
      let$ files = Lwd.get Vars.ui_state.jj_change_files in
      files
      |> List.map (fun (_modifier, file) ->
        Wd.{ data = file; ui = Wd.selectable_item (W.string file) })
    in
    Wd.selection_list_custom
      ~on_selection_change:(Lwd.set selected_file)
      ~custom_handler:(fun _ _ key ->
        match key with `ASCII k, [] -> handleInputs command_mapping k | _ -> `Unhandled)
      file_uis
  ;;

  (**Get the status for the currently selected file*)
  let file_status () =
    let$ selected = Lwd.get selected_file in
    jj_no_log [ "diff"; selected ]
  ;;
end
