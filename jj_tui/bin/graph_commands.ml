open Jj_tui.Logging

module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Notty
  open Jj_tui
  open Nottui
  open! Jj_tui.Util
  open Jj_commands.Shared
  open Jj_commands.Make (Vars)
  open Jj_widgets.Make (Vars)
  module Process = Jj_process.Make (Vars)
  open Process
  open Jj_tui.Process_wrappers.Make (Process)

  (* Helper functions from graph_view *)
  let bookmark_select_prompt get_bookmark_list name func =
    Selection_prompt
      ( name
      , (fun () -> get_bookmark_list () |> Lwd.pure)
      , (fun x bookmark_name -> bookmark_name |> Base.String.is_substring ~substring:x)
      , func )
  ;;

  let custom_commit ?(edit = true) msg =
    let rev = Vars.get_hovered_rev () in
    jj [ "describe"; "-r"; rev; "-m"; msg ] |> ignore;
    (jj @@ [ "new"; "--insert-after"; rev ] @ if edit then [] else [ "--no-edit" ])
    |> ignore
  ;;

  (* Define all graph commands *)
  let get_command_registry get_commands =
    [
      {
        id = "show_help"
      ; description = "Show help"
      ; sorting_key = 0.0
      ; make_cmd =
          (fun () ->
            Fun
              (fun _ ->
                ui_state.show_popup
                $= Some (commands_list_ui ~include_arrows:true (get_commands ()), "Help");
                ui_state.input $= `Mode (fun _ -> `Unhandled)))
      }
    ; {
        id = "prev"
      ; sorting_key = 1.0
      ; description = "Move the working copy to the previous child"
      ; make_cmd = (fun () -> Cmd [ "prev" ])
      }
    ; {
        id = "new_base"
      ; sorting_key = 2.0
      ; description = "Make new child commit"
      ; make_cmd = (fun () -> Cmd_with_revs (Active [ "new" ]))
      }
    ; {
        id = "new_no_edit"
      ; sorting_key = 3.0
      ; description = "Same as 'new', but without editing the new commit"
      ; make_cmd = (fun () -> Cmd_with_revs (Active [ "new"; "--no-edit" ]))
      }
    ; {
        id = "new_inline"
      ; sorting_key = 4.0
      ; description = "Make a new change and insert it after the selected rev"
      ; make_cmd =
          (fun () ->
            Dynamic
              (fun () -> Cmd ([ "new"; "--insert-after" ] @ Vars.get_active_revs ())))
      }
    ; {
        id = "new_inline_no_edit"
      ; sorting_key = 5.0
      ; description = "Same as 'new insert', but without editing the new commit"
      ; make_cmd =
          (fun () ->
            Dynamic
              (fun () ->
                Cmd ([ "new"; "--no-edit"; "--insert-after" ] @ Vars.get_active_revs ())))
      }
    ; {
        id = "duplicate"
      ; sorting_key = 6.0
      ; description = "Duplicate the current selected commits "
      ; make_cmd =
          (fun () -> Dynamic (fun () -> Cmd ([ "duplicate" ] @ Vars.get_active_revs ())))
      }
    ; {
        id = "undo"
      ; sorting_key = 7.0
      ; description = "Undo the last operation"
      ; make_cmd = (fun () -> Cmd [ "undo" ])
      }
    ; {
        id = "commit_base"
      ; sorting_key = 8.0
      ; description =
          "Describe this change and start working on a new rev (same as `describe` then \
           `new`)"
      ; make_cmd =
          (fun () ->
            PromptThen ("commit msg", fun msg -> Fun (fun () -> custom_commit msg)))
      }
    ; {
        id = "commit_no_edit"
      ; sorting_key = 9.0
      ; description = "Same as commit but without editing the new commit"
      ; make_cmd =
          (fun () ->
            PromptThen
              ("commit msg", fun msg -> Fun (fun () -> custom_commit ~edit:false msg)))
      }
    ; {
        id = "split"
      ; sorting_key = 10.0
      ; description = "Split the current commit interacively"
      ; make_cmd = (fun () -> Dynamic_r (fun rev -> Cmd_I [ "split"; "-r"; rev; "-i" ]))
      }
    ; {
        id = "squash_into_parent"
      ; sorting_key = 11.0
      ; description = "Squash into parent"
      ; make_cmd =
          (fun () ->
            Fun
              (fun _ ->
                let rev = Vars.get_hovered_rev () in
                let source_msg, dest_msg = get_messages rev (rev ^ "-") in
                let new_msg = [ dest_msg; source_msg ] |> String.concat_non_empty "\n" in
                jj [ "squash"; "--quiet"; "-r"; rev; "-m"; new_msg ] |> ignore))
      }
    ; {
        id = "squash_into_rev"
      ; sorting_key = 12.0
      ; description = "Squash into any commit"
      ; make_cmd =
          (fun () ->
            PromptThen
              ( "target revision"
              , fun target ->
                  Dynamic_r
                    (fun rev ->
                      let src_msg, dest_msg = get_messages rev target in
                      let new_msg =
                        [ dest_msg; src_msg ] |> String.concat_non_empty "\n"
                      in
                      Cmd
                        [
                          "squash"
                        ; "--quiet"
                        ; "-m"
                        ; new_msg
                        ; "--from"
                        ; rev
                        ; "--into"
                        ; target
                        ]) ))
      }
    ; {
        id = "squash_unsquash"
      ; sorting_key = 13.0
      ; description = "Interactivaly unsquash"
      ; make_cmd =
          (fun () -> Dynamic_r (fun rev -> Cmd_I [ "unsquash"; "-r"; rev; "-i" ]))
      }
    ; {
        id = "squash_interactive_parent"
      ; sorting_key = 14.0
      ; description = "Interactively choose what to squash into parent"
      ; make_cmd = (fun () -> Dynamic_r (fun rev -> Cmd_I [ "squash"; "-r"; rev; "-i" ]))
      }
    ; {
        id = "squash_interactive_rev"
      ; sorting_key = 15.0
      ; description = "Interactively choose what to squash into a commit"
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                Prompt_I ("target revision", [ "squash"; "-i"; "--from"; rev; "--into" ])))
      }
    ; {
        id = "edit"
      ; sorting_key = 16.0
      ; description = "Edit the selected revision"
      ; make_cmd = (fun () -> Dynamic_r (fun rev -> Cmd [ "edit"; rev ]))
      }
    ; {
        id = "describe"
      ; sorting_key = 17.0
      ; description = "Describe this revision"
      ; make_cmd =
          (fun () ->
            Dynamic_r (fun rev -> Prompt ("description", [ "describe"; "-r"; rev; "-m" ])))
      }
    ; {
        id = "describe_editor"
      ; sorting_key = 18.0
      ; description = "Describe this revision using an editor"
      ; make_cmd = (fun () -> Dynamic_r (fun rev -> Cmd_I [ "describe"; "-r"; rev ]))
      }
    ; {
        id = "resolve"
      ; sorting_key = 19.0
      ; description = "Resolve conflicts at this revision"
      ; make_cmd = (fun () -> Dynamic_r (fun rev -> Cmd_I [ "resolve"; "-r"; rev ]))
      }
    ; {
        id = "rebase_single"
      ; sorting_key = 20.0
      ; description = "Rebase single revision "
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev -> Prompt ("Dest rev for " ^ rev, [ "rebase"; "-r"; rev; "-d" ])))
      }
    ; {
        id = "rebase_with_descendants"
      ; sorting_key = 21.0
      ; description = "Rebase revision and its decendents"
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                Prompt
                  ( Printf.sprintf "Dest rev for %s and it's decendents" rev
                  , [ "rebase"; "-s"; rev; "-d" ] )))
      }
    ; {
        id = "rebase_with_bookmark"
      ; sorting_key = 22.0
      ; description = "Rebase revision and all other revissions on its bookmark"
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                Prompt
                  ("Dest rev for bookmark including " ^ rev, [ "rebase"; "-b"; rev; "-d" ])))
      }
    ; {
        id = "git_push"
      ; sorting_key = 23.0
      ; description = "git push"
      ; make_cmd =
          (fun () ->
            Fun
              (fun _ ->
                let revs = Vars.get_active_revs () in
                let subcmds =
                  [
                    {
                      key = Key.key_of_string_exn "y"
                      ; sort_key = 0.0
                    ; description = "proceed"
                    ; cmd =
                        Cmd
                          ([ "git"; "push"; "--allow-new" ]
                           @ (revs |> List.concat_map (fun x -> [ "-r"; x ])))
                    }
                  ; {
                      key = Key.key_of_string_exn "n"
                    ; sort_key = 1.0
                    ; description = "exit"
                    ; cmd =
                        Fun
                          (fun _ ->
                            ui_state.input $= `Normal;
                            ui_state.show_popup $= None)
                    }
                  ]
                  |> List.map (fun x -> x.key, x)
                  |> Key_map.Key_Map.of_list
                in
                let log =
                  jj_no_log
                    ~get_stderr:true
                    ([ "git"; "push"; "--allow-new"; "--dry-run" ]
                     @ (revs |> List.concat_map (fun x -> [ "-r"; x ])))
                  |> AnsiReverse.colored_string
                  |> Ui.atom
                  |> Lwd.pure
                in
                let ui = W.vbox [ log; commands_list_ui subcmds ] in
                ui_state.show_popup $= Some (ui, "Git push will:");
                ui_state.input $= `Mode (command_input ~is_sub:true subcmds)))
      }
    ; {
        id = "git_fetch"
      ; sorting_key = 24.0
      ; description = "git fetch"
      ; make_cmd = (fun () -> Cmd [ "git"; "fetch" ])
      }
    ; {
        id = "git_fetch_all"
      ; sorting_key = 25.0
      ; description = "git fetch all remotes"
      ; make_cmd = (fun () -> Cmd [ "git"; "fetch"; "--all-remotes" ])
      }
    ; {
        id = "parallelize"
      ; sorting_key = 26.0
      ; description =
          "Parallelize commits. Takes 2 commits and makes them have the\n\
           same parent and child. Run `jj parallelize` --help for details"
      ; make_cmd =
          (fun () ->
            PromptThen
              ( "list commits to parallelize"
              , fun x -> Cmd ([ "paralellize" ] @ (x |> String.split_on_char ' ')) ))
      }
    ; {
        id = "abandon"
      ; sorting_key = 27.0
      ; description = "Abandon this change(removes just this change and rebases parents)"
      ; make_cmd =
          (fun () ->
            Dynamic
              (fun () ->
                let revs = Vars.get_active_revs () in
                Cmd ([ "abandon" ] @ revs)
                |> confirm_prompt
                     ("abandon the revisions:\n" ^ (revs |> String.concat "\n"))))
      }
    ; {
        id = "bookmark_create"
      ; sorting_key = 28.0
      ; description = "Create new bookmark"
      ; make_cmd =
          (fun () ->
            PromptThen
              ( "Bookmark name to create"
              , fun x ->
                  Cmd_r
                    ([ "bookmark"; "create" ]
                     @ [ x |> String.map (fun c -> if c = ' ' then '_' else c) ]) ))
      }
    ; {
        id = "bookmark_delete"
      ; sorting_key = 29.0
      ; description = "Delete bookmark"
      ; make_cmd =
          (fun () ->
            bookmark_select_prompt
              branches_no_remote
              "Bookmark to delete"
              (fun bookmark ->
                 Cmd [ "bookmark"; "delete"; bookmark ]
                 |> confirm_prompt
                      (Printf.sprintf
                         "delete the bookmark: '%s' This will also delete it on the \
                          remote next \"git push\"."
                         bookmark)))
      }
    ; {
        id = "bookmark_forget"
      ; sorting_key = 30.0
      ; description = "Forget bookmark"
      ; make_cmd =
          (fun () ->
            bookmark_select_prompt
              branches_no_remote
              "Bookmark to forget"
              (fun bookmark ->
                 Cmd [ "bookmark"; "forget"; bookmark ]
                 |> confirm_prompt
                      (Printf.sprintf
                         "forget the bookmark: '%s' . This will not delete it on the \
                          remote."
                         bookmark)))
      }
    ; {
        id = "bookmark_rename"
      ; sorting_key = 31.0
      ; description = "Rename bookmark"
      ; make_cmd =
          (fun () ->
            bookmark_select_prompt
              branches_no_remote
              "Select the bookmark to rename (only local/tracked bookmarks are shown)"
              (fun curr_name ->
                 Prompt ("New bookmark name", [ "bookmark"; "rename"; curr_name ])))
      }
    ; {
        id = "bookmark_set"
      ; sorting_key = 32.0
      ; description = "Set bookmark to this change"
      ; make_cmd =
          (fun () ->
            Dynamic_r
              (fun rev ->
                bookmark_select_prompt
                  branches_no_remote
                  ("Select the bookmark to set to rev: " ^ rev)
                  (fun bookmark ->
                     Cmd [ "bookmark"; "set"; "-r"; get_hovered_rev (); "-B"; bookmark ])))
      }
    ; {
        id = "bookmark_track"
      ; sorting_key = 33.0
      ; description = "track given remote bookmark"
      ; make_cmd =
          (fun () ->
            bookmark_select_prompt
              branches_remotes_not_tracked
              "Select the bookmark to begin tracking"
              (fun bookmark -> Cmd [ "bookmark"; "track"; bookmark ]))
      }
    ; {
        id = "bookmark_untrack"
      ; sorting_key = 34.0
      ; description = "untrack given remote bookmark"
      ; make_cmd =
          (fun () ->
            bookmark_select_prompt
              branches_remotes_tracked
              "Select the bookmark to untrack"
              (fun bookmark -> Cmd [ "bookmark"; "untrack"; bookmark ]))
      }
    ; {
        id = "filter"
      ; sorting_key = 35.0
      ; description = "Filter using revset"
      ; make_cmd =
          (fun () ->
            PromptThen
              ( "Filter using revset"
              , fun revset ->
                  Fun
                    (fun () ->
                      if revset = ""
                      then Vars.ui_state.revset $= None
                      else Vars.ui_state.revset $= Some revset) ))
      }
    ; {
        id = "absorb"
      ; sorting_key = 36.0
      ; description =
          "Absorb: Move changes of each file in this commit into the closest mutable \
           parent that modified that file"
      ; make_cmd = (fun () -> Cmd_r [ "absorb"; "--from"; ])
      }
    ]
    |> List.to_seq
    |> Seq.map (fun x -> x.id, x)
    |> Hashtbl.of_seq
  ;;
end