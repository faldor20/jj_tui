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

  let selected_file = Lwd.var ""

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
                  [
                    "squash"
                  ; "-u"
                  ; "--keep-emptied"
                  ; "--from"
                  ; get_selected_rev ()
                  ; "--into"
                  ; rev
                  ; Lwd.peek selected_file
                  ] )
      }
    ; {
        key = 'N'
      ; description = "Move file to child commit"
      ; cmd =
          Dynamic_r
            (fun rev ->
              Cmd
                [
                  "squash"
                ; "-u"
                ; "--keep-emptied"
                ; "--from"
                ; rev
                ; "--into"
                ; rev ^ "+"
                ; Lwd.peek selected_file
                ])
      }
    ; {
        key = 'P'
      ; description = "Move file to parent commit"
      ; cmd =
          Dynamic_r
            (fun rev ->
              Cmd
                [
                  "squash"
                ; "-u"
                ; "--keep-emptied"
                ; "--from"
                ; rev
                ; "--into"
                ; rev ^ "-"
                ; Lwd.peek selected_file
                ])
      }
    ; {
        key = 'd'
      ; description = "Restore to previous revision (git discard)"
      ; cmd =
          Dynamic_r
            (fun rev ->
              let selected = Lwd.peek selected_file in
              confirm_prompt
                ("discard all changes to '" ^ selected ^ "' in rev " ^ rev)
                (Cmd [ "restore"; "--to"; rev; "--from"; rev ^ "-"; selected ]))
      }
    ]
  ;;

  let file_view focus =
    let file_uis =
      let$ files = Lwd.get Vars.ui_state.jj_change_files in
      files
      |> List.map (fun (_modifier, file) ->
        W.Lists.{ data = file; ui = W.Lists.selectable_item (W.string file) })
    in
    let show_diff_promise = ref @@ Promise.of_value () in
    (*TODO: 
    This should be redesigned completely
    There will be a new function that renders the show state
    It will have a cancellation system just like this one. 
    when any of the dependencies change, selected file, selected rev, focus etc, it will re-render if needed and cancel the current rendering.


    *)
    let show_selected_file_diff ()=
      (* kill any existing process writing to the show buffer*)
      !show_diff_promise |> Promise.terminate;
      (* set self as current process writing to show buffer*)
      show_diff_promise
      := Picos_std_structured.Flock.fork_as_promise (fun () ->
           let rev = Vars.get_selected_rev () in
           let selected = Lwd.peek selected_file in
           Vars.ui_state.jj_show
           $=
           if selected != ""
           then 
           let log=jj_no_log [ "diff"; "-r"; rev; selected ] in
            Control.yield();
            let res=log|> AnsiReverse.colored_string in
            Control.yield();
            res
           else I.string A.empty "")
    in
    W.Lists.selection_list_custom
      ~on_selection_change:(fun selected ->
        Lwd.set selected_file selected;
        if Focus.peek_has_focus focus then show_selected_file_diff())
      ~custom_handler:(fun _ key ->
        match key with `ASCII k, [] -> handleInputs command_mapping k | _ -> `Unhandled)
      file_uis
  ;;
end
