module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Nottui
  open! Jj_tui.Util
  open Jj_commands.Make (Vars)
  open Global_vars
  open Jj_tui
  open Picos_std_structured

  (* Import file commands *)
  module FileCommands = File_commands.Make (Vars)
  
  open Jj_tui.Key_map
  let active_files = Lwd.var [ "" ]

  (* Remove the hardcoded make_command_mapping function and use the dynamic one *)
  let command_mapping = ref None
  
  let rec get_command_mapping () =
    match !command_mapping with
    | Some mapping -> mapping
    | None ->
      let key_map = (Lwd.peek ui_state.config).key_map.file in
      let registry = FileCommands.get_command_registry active_files get_command_mapping in
      let mapping = build_command_list key_map registry in
      command_mapping := Some mapping;
      mapping
  ;;
  
  let hovered_var = ref "./"

  let file_view ~focus summary_focus =
    let file_uis =
      let$ files = Lwd.get Vars.ui_state.jj_change_files in
      files
      |> List.map (fun (_modifier, file) ->
        W.Lists.
          {
            data = file
          ; id = file |> String.hash
          ; ui = W.Lists.selectable_item (W.string file)
          })
    in
    (*TODO:
      This should be redesigned completely
      There will be a new function that renders the show state
      It will have a cancellation system just like this one.
      when any of the dependencies change, selected file, selected rev, focus etc, it will re-render if needed and cancel the current rendering.
    *)
    let ui =
      file_uis
      |> W.Lists.multi_selection_list_custom
           ~reset_selections:Vars.ui_state.reset_selection
           ~on_selection_change:(fun ~hovered ~selected ->
             let active = if selected |> List.length = 0 then [ hovered ] else selected in
             Lwd.set active_files active;
             hovered_var := hovered;
             if Focus.peek_has_focus focus
             then
               Show_view.(push_status (File_preview (Vars.get_hovered_rev (), hovered))))
           ~custom_handler:(fun ~selected:_ ~selectable_items:_ key ->
             match key with
             | `Enter, [] ->
               Focus.request_reversable summary_focus;
               `Handled
             |  k  ->
              handleInputs (get_command_mapping ()) k
             | _ ->
               `Unhandled)
    in
    let$ ui = ui
    and$ _ =
      Focus.status focus |>$ fun focus ->
      if Focus.has_focus focus
      then Show_view.(push_status (File_preview (Vars.get_hovered_rev (), !hovered_var)))
    in
    ui
  ;;

end
