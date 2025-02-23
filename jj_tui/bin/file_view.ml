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

  open Jj_tui.Key_map
  let active_files = Lwd.var [ "" ]

  let rec make_command_mapping (key_map: Key_map.file_keys) =
    [
      {
        key = key_map.show_help
      ; description = "Show help"
      ; cmd =
          Fun
            (fun _ ->
              ui_state.show_popup
              $= Some (commands_list_ui ~include_arrows:true (get_command_mapping ()), "Help");
              ui_state.input $= `Mode (fun _ -> `Unhandled))
      }
    ; {
        key = key_map.move_to_rev
      ; description = "Move file to other commit"
      ; cmd =
          PromptThen
            ( "Revision to move file to"
            , fun rev ->
                Cmd
                  ([
                     "squash"
                   ; "-u"
                   ; "--keep-emptied"
                   ; "--from"
                   ; get_hovered_rev ()
                   ; "--into"
                   ; rev
                   ]
                   @ Lwd.peek active_files) )
      }
    ; {
        key = key_map.move_to_child
      ; description = "Move file to child commit"
      ; cmd =
          Dynamic_r
            (fun rev ->
              Cmd
                ([ "squash"; "-u"; "--keep-emptied"; "--from"; rev; "--into"; rev ^ "+" ]
                 @ Lwd.peek active_files))
      }
    ; {
        key = key_map.move_to_parent
      ; description = "Move file to parent commit"
      ; cmd =
          Dynamic_r
            (fun rev ->
              Cmd
                ([ "squash"; "-u"; "--keep-emptied"; "--from"; rev; "--into"; rev ^ "-" ]
                 @ Lwd.peek active_files))
      }
    ; {
        key = key_map.discard
      ; description = "Restore to previous revision (git discard)"
      ; cmd =
          Dynamic_r
            (fun rev ->
              let selected = Lwd.peek active_files in
              confirm_prompt
                ("discard all changes to:\n"
                 ^ (selected |> String.concat "\n")
                 ^ "\nin rev "
                 ^ rev)
                (Cmd ([ "restore"; "--to"; rev; "--from"; rev ^ "-" ] @ selected)))
      }
    ]
  and command_mapping = ref None
  and get_command_mapping () =
    match !command_mapping with
    | Some mapping -> mapping
    | None -> 
      let mapping = make_command_mapping (Lwd.peek ui_state.config).key_map.file in
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
