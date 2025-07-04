open Jj_tui.Logging

module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Nottui
  open! Jj_tui.Util
  open Jj_commands.Make (Vars)
  open Jj_commands.Shared
  open Global_vars
  open Jj_tui

  (* Define all file commands *)
  let get_command_registry active_files get_commands =
    [ {
        id = "show_help"
      ; description = "Show help"
      ; sorting_key = 0.0
      ; make_cmd =
          (fun () ->
            Fun
              (fun _ ->
                show_popup
                @@ Some (commands_list_ui ~include_arrows:true (get_commands ()), "Help");
                ui_state.input $= `Mode (fun _ -> `Unhandled)))
      }
    ; {
        id = "move_to_rev"
      ; description = "Move file to other commit"
      ; sorting_key = 1.0
      ; make_cmd =
          (fun () ->
            PromptThen
              ( "Revision to move file to"
              , fun rev ->
                  Cmd
                    ( [ "squash"
                      ; "-u"
                      ; "--keep-emptied"
                      ; "--from"
                      ; get_hovered_rev ()
                      ; "--into"
                      ; rev
                      ]
                    @ Lwd.peek active_files ) ))
      }
    ; {
        id = "move_to_child"
      ; description = "Move file to child commit"
      ; sorting_key = 2.0
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                Cmd
                  ( [ "squash"
                    ; "-u"
                    ; "--keep-emptied"
                    ; "--from"
                    ; rev
                    ; "--into"
                    ; rev ^ "+"
                    ]
                  @ Lwd.peek active_files )))
      }
    ; {
        id = "move_to_parent"
      ; description = "Move file to parent commit"
      ; sorting_key = 3.0
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                Cmd
                  ( [ "squash"
                    ; "-u"
                    ; "--keep-emptied"
                    ; "--from"
                    ; rev
                    ; "--into"
                    ; rev ^ "-"
                    ]
                  @ Lwd.peek active_files )))
      }
    ; {
        id = "commit"
      ; description = "Create a commit with the selected files"
      ; sorting_key = 3.0
      ; make_cmd =
          (fun () ->
            PromptThen
              ( "Commit message"
              , fun message ->
              
                    Fun
                    (fun _-> (* I need this to work with any commit. So i should split then describe instead*)
                      let rev=Vars.get_hovered_rev () in
                      jj
                        ( [ "split";"-r"; rev; "-m";message; "--insert-before"; "@" ]
                        @ Lwd.peek active_files )|>ignore;
                        )))
      }
    ; {
        id = "abandon"
      ; description = "Restore to previous revision (git discard)"
      ; sorting_key = 4.0
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                let selected = Lwd.peek active_files in
                confirm_prompt
                  ("abandon all changes to:\n"
                   ^ (selected |> String.concat "\n")
                   ^ "\nin rev "
                   ^ rev)
                  (Cmd (["restore"; "--to"; rev; "--from"; rev ^ "-"] @ selected))))
      }
    ; {
        id = "absorb"
      ; description = "Move changes from this revision to the nearest parent that modified the same lines"
      ; sorting_key = 5.0
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                let selected = Lwd.peek active_files in
                confirm_prompt
                  ("absorb all changes to:\n"
                   ^ (selected |> String.concat "\n")
                   ^ "\nin rev "
                   ^ rev)
                  (Cmd (["absorb"; "--from"; rev] @ selected))))
      }
    ; {
        id = "absorb-into"
      ; description = "Absorb into a specific revision"
      ; sorting_key = 5.0
      ; make_cmd =
          (fun () ->
            PromptThen
              ( "Revision to move file to"
              , fun dest ->
                Dynamic_r
                  (fun rev ->
                    let selected = Lwd.peek active_files in
                    confirm_prompt
                      ("absorb all changes to:\n"
                       ^ (selected |> String.concat "\n")
                       ^ "\nin rev "
                       ^ rev)
                      (Cmd (["absorb"; "--from"; rev;"--to"; dest] @ selected)))))
      }
    ; {
        id = "undo"
      ; sorting_key = 7.0
      ; description = "Undo the last operation"
      ; make_cmd = (fun () -> Cmd [ "undo" ])
      }
    ]
    |> List.to_seq
    |> Seq.map (fun x -> x.id, x)
    |> Hashtbl.of_seq
end 
