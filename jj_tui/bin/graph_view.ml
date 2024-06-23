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
  open Jj_widgets.Make (Vars)

  let rec command_mapping : command list =
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
        key = 'P'
      ; description = "Move the working copy to the previous child "
      ; cmd = Cmd [ "prev" ]
      }
    ; {
        key = 'p'
      ; description = "Edit the previous child change"
      ; cmd = Cmd [ "prev"; "--edit" ]
      }
    ; {
        key = 'N'
      ; description = "Move the working copy to the next child "
      ; cmd = Cmd [ "next" ]
      }
    ; {
        key = 'n'
      ; description = "Edit the next child change"
      ; cmd = Cmd [ "next"; "--edit" ]
      }
    ; {
        key = 'i'
      ; cmd =
          SubCmd
            [
              {
                key = 'i'
              ; description = "Make a new empty change"
              ; cmd = Cmd_r [ "new" ]
              }
            ; {
                key = 'a'
              ; description = "Insert a new empty change after a specific revision"
              ; cmd = Prompt ("New change after commit:", [ "new" ])
              }
            ]
      ; description = "Make a new empty change"
      }
    ; {
        key = 'c'
      ; description = "Describe this change and move on (same as `describe` then `new`) "
      ; cmd = Prompt ("commit msg", [ "commit"; "-m" ])
      }
    ; {
        key = 'S'
      ; description = "Split the current commit interacively"
      ; cmd = Cmd_I [ "split"; "-i" ]
      }
    ; {
        key = 's'
      ; description = "Squash/unsquash (has subcommands)"
      ; cmd =
          SubCmd
            [
              {
                key = 'S'
              ; cmd = Cmd_I [ "unsquash"; "-i" ]
              ; description = "Interactivaly unsquash"
              }
            ; {
                key = 's'
              ; description = "Squash into parent"
              ; cmd =
                  Fun
                    (fun _ ->
                      let curr_msg, prev_msg = get_messages () in
                      let new_msg = prev_msg ^ curr_msg in
                      jj [ "squash"; "--quiet"; "-r";Lwd.peek Vars.ui_state.selected_revision; "-m"; new_msg ] |> ignore)
              }
            ; {
                key = 'S'
              ; description = "Squash into any commit"
              ; cmd =
                  PromptThen
                    ( "target revision"
                    , fun str ->
                        let curr_msg, prev_msg = get_messages () in
                        let new_msg = prev_msg ^ curr_msg in
                        Cmd [ "squash"; "--quiet"; "-m"; new_msg; "--into"; str ] )
              }
            ; {
                key = 'i'
              ; description = "Interactively choose what to squash into parent"
              ; cmd = Cmd_I [ "squash"; "-i" ]
              }
            ; {
                key = 'I'
              ; description = "Interactively choose what to squash into a commit"
              ; cmd = Prompt_I ("target revision", [ "squash"; "-i"; "--into" ])
              }
            ]
      }
    ; {
        key = 'e'
      ; cmd =
          SubCmd
            [
              {
                key = 'e'
              ; cmd =
                  Dynamic
                    (fun () -> Cmd [ "edit"; Lwd.peek Vars.ui_state.selected_revision ])
              ; description = "Edit the selected revision"
              }
            ; {
                key = 'r'
              ; cmd = Prompt ("revision", [ "edit" ])
              ; description = "Edit a specific revision"
              }
            ]
      ; description = "Edit a revision"
      }
    ; {
        key = 'd'
      ; cmd =
          Dynamic
            (fun () ->
              Prompt
                ( "description"
                , [ "describe"; "-r"; Lwd.peek Vars.ui_state.selected_revision; "-m" ] ))
      ; description = "Describe this revision"
      }
    ; {
        key = 'R'
      ; cmd = Cmd_I [ "resolve" ]
      ; description = "Resolve conflicts at this revision"
      }
    ; {
        key = 'r'
      ; description = "Rebase revision "
      ; cmd =
          SubCmd
            [
              {
                key = 'r'
              ; description = "Rebase single revision "
              ; cmd =
                  Dynamic
                    (fun () ->
                    let rev=Lwd.peek Vars.ui_state.selected_revision  in
                      Prompt
                        ( "Dest rev for " ^rev
                        , [
                            "rebase"; "-r"; rev; "-d"
                          ] ))
              }
            ; {
                key = 's'
              ; description = "Rebase revision and its decendents"
              ; cmd =
                  Dynamic
                    (fun () ->
                    let rev=Lwd.peek Vars.ui_state.selected_revision  in
                      Prompt
                        ( Printf.sprintf "Dest rev for %s and it's decendents" rev 
                        , [
                            "rebase"; "-s"; rev; "-d"
                          ] ))
              }
            ; {
                key = 'b'
              ; description = "Rebase revision and all other revissions on its branch"
              ; cmd =
                  Dynamic
                    (fun () ->
                    let rev=Lwd.peek Vars.ui_state.selected_revision  in
                      Prompt
                        ( "Dest rev for branch including "^rev 
                        , [
                            "rebase"; "-b"; rev; "-d"
                          ] ))
              }
            ]
      }
    ; {
        key = 'g'
      ; description = "Git commands"
      ; cmd =
          SubCmd
            [
              { key = 'p'; description = "git push"; cmd = Cmd [ "git"; "push" ] }
            ; { key = 'f'; description = "git fetch"; cmd = Cmd [ "git"; "fetch" ] }
            ]
      }
    ; {
        key = 'z'
      ; description =
          "Parallelize commits. Takes 2 commits and makes them have the\n\
           same parent and child. Run `jj parallelize` --help for details"
      ; cmd =
          PromptThen
            ( "list commits to parallelize"
            , fun x -> Cmd ([ "paralellize" ] @ (x |> String.split_on_char ' ')) )
      }
    ; {
        key = 'a'
      ; description = "Abandon this change(removes just this change and rebases parents)"
      ; cmd =
          Dynamic
            (fun () ->
              Cmd_r [ "abandon" ]
              |> confirm_prompt
                   ("abandon the revision:" ^ Lwd.peek Vars.ui_state.selected_revision))
      }
    ; {
        key = 'b'
      ; description = "Branch commands"
      ; cmd =
          SubCmd
            [
              {
                key = 'c'
              ; description = "Create new branches"
              ; cmd =
                  PromptThen
                    ( "Branch names to create"
                    , fun x ->
                        Cmd ([ "branch"; "create" ] @ (x |> String.split_on_char ' ')) )
              }
            ; {
                key = 'd'
              ; description = "Delete branches"
              ; cmd = Prompt ("Branch names to delete", [ "branch"; "delete" ])
              }
            ; {
                key = 'r'
              ; description = "Rename branch"
              ; cmd =
                  PromptThen
                    ( "Branch to rename"
                    , fun curr_name ->
                        Prompt ("New branch name", [ "branch"; "rename"; curr_name ]) )
              }
            ; {
                key = 's'
              ; description = "set branch to this change"
              ; cmd =
                  Dynamic
                    (fun () ->
                      Prompt
                        ( "Branch to set to this commit "
                        , [
                            "branch"
                          ; "set"
                          ; "-r"
                          ; Lwd.peek Vars.ui_state.selected_revision
                          ; "-B"
                          ] ))
              }
            ; {
                key = 't'
              ; description = "track given remote branch"
              ; cmd = Prompt ("Branch to track 'branch@remote'", [ "branch"; "track" ])
              }
            ; {
                key = 'u'
              ; description = "untrack given remote branch"
              ; cmd = Prompt ("Branch to untrack 'branch@remote'", [ "branch"; "untrack" ])
              }
            ]
      }
    ]
  ;;

  (*TODO:make a custom widget the renders the commit with and without selection.
    with selection replace the dot with a blue version and slightly blue tint the background *)
  let graph_view ~sw () =
    let ui =
      let$ graph, rev_ids =
        (*TODO I think this ads a slight delay to everything becasue it makes things need to be renedered twice. maybe I could try getting rid of it*)
        Vars.ui_state.trigger_update |> Lwd.get |> Lwd.map ~f:(fun _ -> seperate_revs ())
      in
      let selectable_idx = ref 0 in
      graph
      |> Array.map (fun x ->
        match x with
        | `Selectable x ->
          let ui is_focused =
            (*hightlight blue when selection is true*)
            let prefix =
              if is_focused then I.char A.(bg A.blue) '>' 1 2 else I.char A.empty ' ' 1 2
            in
            I.hcat
              [
                prefix
              ; x ^ "\n"
                (* TODO This won't work if we are on a branch, because that puts the @ further out*)
                |> Jj_tui.AnsiReverse.colored_string
              ]
            |> Ui.atom
          in
          let data = Wd.{ ui; data = rev_ids.(!selectable_idx) } in
          selectable_idx := !selectable_idx + 1;
          Wd.(Selectable data)
        | `Filler x ->
          Wd.(Filler (" " ^ x ^ "\n" |> Jj_tui.AnsiReverse.colored_string |> Ui.atom)))
    in
    ui
    |> Wd.selection_list_exclusions
         ~on_selection_change:(fun revision ->
           Eio.Fiber.fork ~sw @@ fun _ ->
           Vars.update_ui_state @@ fun _ ->
           Lwd.set Vars.ui_state.selected_revision revision;
           Global_funcs.update_views ())
         ~custom_handler:(fun _ _ key ->
           match key with
           | `ASCII k, [] ->
             handleInputs command_mapping k
           | _ ->
             `Unhandled)
  ;;
end
