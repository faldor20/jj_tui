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

  let active_files= Lwd.var [""]

  let rec command_mapping =
    [
      {
        key = '?'
      ; description = "Show help"
      ; cmd =
          Fun
            (fun _ ->
              ui_state.show_popup
              $= Some (commands_list_ui ~include_arrows:true command_mapping, "Help");
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
                  ([
                    "squash"
                  ; "-u"
                  ; "--keep-emptied"
                  ; "--from"
                  ; get_hovered_rev ()
                  ; "--into"
                  ; rev
                  ]
                  @
                   (Lwd.peek active_files))
                   )
      }
    ; {
        key = 'N'
      ; description = "Move file to child commit"
      ; cmd =
          Dynamic_r
            (fun rev ->
              Cmd (
                [
                  "squash"
                ; "-u"
                ; "--keep-emptied"
                ; "--from"
                ; rev
                ; "--into"
                ; rev ^ "+"
                ]@
                 Lwd.peek active_files
                 )
                )
      }
    ; {
        key = 'P'
      ; description = "Move file to parent commit"
      ; cmd =
          Dynamic_r
            (fun rev ->
              Cmd(
                [
                  "squash"
                ; "-u"
                ; "--keep-emptied"
                ; "--from"
                ; rev
                ; "--into"
                ; rev ^ "-"
                ]@
                 Lwd.peek active_files
                 )
                )
      }
    ; {
        key = 'd'
      ; description = "Restore to previous revision (git discard)"
      ; cmd =
          Dynamic_r
            (fun rev ->
              let selected = Lwd.peek active_files in
              confirm_prompt
                ("discard all changes to '" ^ (selected|>String.concat "\n") ^ "' in rev " ^ rev)
                (Cmd (["restore"; "--to"; rev; "--from"; rev ^ "-"] @selected)))
      }
    ]
  ;;

  let file_view focus =
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
    file_uis|>
    W.Lists.multi_selection_list_custom
      ~on_selection_change:(fun ~hovered ~selected ->
      let active=
        if selected|>List.length =0 then [hovered] else selected
        in
        Lwd.set active_files active;
        if Focus.peek_has_focus focus
        then Show_view.(pushStatus (File_preview (Vars.get_hovered_rev (), hovered))))
      ~custom_handler:(fun ~selected:_ ~selectable_items:_ key ->
        match key with `ASCII k, [] -> handleInputs command_mapping k | _ -> `Unhandled)
  ;;
end
