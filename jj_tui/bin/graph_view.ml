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
        key = 'N'
      ; description = "Move the working copy to the next child "
      ; cmd = Cmd [ "next" ]
      }
    ; {
        key = 'n'
      ; cmd = Dynamic_r (fun rev -> Cmd [ "new"; rev ])
      ; description = "Make a new empty change as a child of the selected rev"
      }
    ; {
        key = 'c'
      ; description =
          "Describe this change and start working on a new rev (same as `describe` then \
           `new`) "
      ; cmd = Prompt ("commit msg", [ "commit"; "-m" ])
      }
    ; {
        key = 'S'
      ; description = "Split the current commit interacively"
      ; cmd = Dynamic_r (fun rev -> Cmd_I [ "split"; "-r"; rev; "-i" ])
      }
    ; {
        key = 's'
      ; description = "Squash/unsquash (has subcommands)"
      ; cmd =
          SubCmd
            [
              {
                key = 's'
              ; description = "Squash into parent"
              ; cmd =
                  Fun
                    (fun _ ->
                      let curr_msg, prev_msg = get_messages () in
                      let new_msg = prev_msg ^ curr_msg in
                      let rev = Vars.get_selected_rev () in
                      jj [ "squash"; "--quiet"; "-r"; rev; "-m"; new_msg ] |> ignore)
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
                        Dynamic_r
                          (fun rev ->
                            Cmd
                              [
                                "squash"
                              ; "--quiet"
                              ; "-m"
                              ; new_msg
                              ; "--from"
                              ; rev
                              ; "--into"
                              ; str
                              ]) )
              }
            ; {
                key = 'u'
              ; cmd = Dynamic_r (fun rev -> Cmd_I [ "unsquash"; "-r"; rev; "-i" ])
              ; description = "Interactivaly unsquash"
              }
            ; {
                key = 'i'
              ; description = "Interactively choose what to squash into parent"
              ; cmd = Dynamic_r (fun rev -> Cmd_I [ "squash"; "-r"; rev; "-i" ])
              }
            ; {
                key = 'I'
              ; description = "Interactively choose what to squash into a commit"
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      Prompt_I
                        ("target revision", [ "squash"; "-i"; "--from"; rev; "--into" ]))
              }
            ]
      }
    ; {
        key = 'e'
      ; cmd = Dynamic_r (fun rev -> Cmd [ "edit"; rev ])
      ; description = "Edit the selected revision"
      }
    ; {
        key = 'd'
      ; cmd =
          Dynamic_r (fun rev -> Prompt ("description", [ "describe"; "-r"; rev; "-m" ]))
      ; description = "Describe this revision"
      }
    ; {
        key = 'R'
      ; cmd = Dynamic_r (fun rev -> Cmd_I [ "resolve"; "-r"; rev ])
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
                  Dynamic_r
                    (fun rev ->
                      Prompt ("Dest rev for " ^ rev, [ "rebase"; "-r"; rev; "-d" ]))
              }
            ; {
                key = 's'
              ; description = "Rebase revision and its decendents"
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      Prompt
                        ( Printf.sprintf "Dest rev for %s and it's decendents" rev
                        , [ "rebase"; "-s"; rev; "-d" ] ))
              }
            ; {
                key = 'b'
              ; description = "Rebase revision and all other revissions on its branch"
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      Prompt
                        ( "Dest rev for branch including " ^ rev
                        , [ "rebase"; "-b"; rev; "-d" ] ))
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
          Dynamic_r
            (fun rev ->
              Cmd_r [ "abandon" ] |> confirm_prompt ("abandon the revision:" ^ rev))
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
                        Cmd_r ([ "branch"; "create" ] @ (x |> String.split_on_char ' '))
                    )
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
                  Dynamic_r
                    (fun rev ->
                      Prompt
                        ( "Branch to set to this commit "
                        , [ "branch"; "set"; "-r"; rev; "-B" ] ))
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
        Vars.ui_state.trigger_update |> Lwd.get |> Lwd.map ~f:(fun _ -> graph_and_revs ())
      in
      let selectable_idx = ref 0 in
      graph
      |> Array.map (fun x ->
        match x with
        | `Selectable x ->
          let ui =
            Wd.selectable_item
              (x ^ "\n"
               (* TODO This won't work if we are on a branch, because that puts the @ further out*)
               |> Jj_tui.AnsiReverse.colored_string
               |> Ui.atom)
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
