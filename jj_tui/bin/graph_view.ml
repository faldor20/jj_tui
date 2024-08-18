module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Jj_tui
  open Nottui
  open! Jj_tui.Util
  open Jj_commands.Make (Vars)
  open Jj_widgets.Make (Vars)

  let branch_select_prompt get_branch_list name func =
    Selection_prompt
      ( name
      , (fun () -> get_branch_list () |> Lwd.pure)
      , (fun x branch_name -> branch_name |> Base.String.is_substring ~substring:x)
      , func )
  ;;

  let rec command_mapping : 'acommand list =
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
        key = 'P'
      ; description = "Move the working copy to the previous child "
      ; cmd = Cmd [ "prev" ]
      }
    ; {
        key = 'N'
      ; description = "Make a new change and insert it after the selected rev"
      ; cmd = Dynamic_r (fun rev -> Cmd [ "new"; "--insert-after"; rev ])
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
                      let rev = Vars.get_selected_rev () in
                      let source_msg, dest_msg = get_messages rev (rev ^ "-") in
                      let new_msg =
                        [ dest_msg;source_msg  ] |> String.concat_non_empty "\n"
                      in
                      jj [ "squash"; "--quiet"; "-r"; rev; "-m"; new_msg ] |> ignore)
              }
            ; {
                key = 'S'
              ; description = "Squash into any commit"
              ; cmd =
                  PromptThen
                    ( "target revision"
                    , fun target ->
                        Dynamic_r
                          (fun rev ->
                            let src_msg, dest_msg = get_messages rev target in
                            let new_msg =
                              [  dest_msg;src_msg ] |> String.concat_non_empty "\n"
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
              {
                key = 'p'
              ; description = "git push"
              ; cmd =
                  Fun
                    (fun _ ->
                      let subcmds =
                        [
                          {
                            key = 'y'
                          ; description = "proceed"
                          ; cmd = Cmd [ "git"; "push" ]
                          }
                        ; {
                            key = 'n'
                          ; description = "exit"
                          ; cmd =
                              Fun
                                (fun _ ->
                                  ui_state.input $= `Normal;
                                  ui_state.show_popup $= None)
                          }
                        ]
                      in
                      let log =
                        jj_no_log   ~get_stderr:true [ "git"; "push"; "--dry-run" ]
                        |> AnsiReverse.colored_string
                        |> Ui.atom
                        |> Lwd.pure
                      in
                      let ui = W.vbox [ log; commands_list_ui subcmds ] in
                      ui_state.show_popup $= Some (ui, "Git push will:");
                      ui_state.input $= `Mode (command_input ~is_sub:true subcmds))
              }
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
              ; description = "Delete branch"
              ; cmd =
                  branch_select_prompt
                    branches_no_remote
                    "Branch to delete"
                    (fun branch ->
                       Cmd [ "branch"; "delete"; branch ]
                       |> confirm_prompt
                            (Printf.sprintf
                               "delete the branch: '%s' This will also delete it on the \
                                remote next \"git push\"."
                               branch))
              }
            ; {
                key = 'f'
              ; description = "Forget branch"
              ; cmd =
                  branch_select_prompt
                    branches_no_remote
                    "Branch to forget"
                    (fun branch ->
                       Cmd [ "branch"; "forget"; branch ]
                       |> confirm_prompt
                            (Printf.sprintf
                               "orget the branch: '%s' . This will not delete it on the \
                                remote."
                               branch))
              }
            ; {
                key = 'r'
              ; description = "Rename branch"
              ; cmd =
                  branch_select_prompt
                    branches_no_remote
                    "Select the branch to rename (only local/tracked branches are shown)"
                    (fun curr_name ->
                       Prompt ("New branch name", [ "branch"; "rename"; curr_name ]))
              }
            ; {
                key = 's'
              ; description = "Set branch to this change"
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      branch_select_prompt
                        branches_no_remote
                        ("Select the branch to set to rev: " ^ rev)
                        (fun branch ->
                           Cmd
                             [ "branch"; "set"; "-r"; get_selected_rev (); "-B"; branch ]))
              }
            ; {
                key = 't'
              ; description = "track given remote branch"
              ; cmd =
                  branch_select_prompt
                    branches_remotes_not_tracked
                    "Select the branch to begin tracking"
                    (fun branch -> Cmd [ "branch"; "track"; "-B"; branch ])
              }
            ; {
                key = 'u'
              ; description = "untrack given remote branch"
              ; cmd =
                  branch_select_prompt
                    branches_remotes_tracked
                    "Select the branch to untrack"
                    (fun branch -> Cmd [ "branch"; "untrack"; "-B"; branch ])
              }
            ]
      }
    ; {
        key = 'f'
      ; description = "Filter using revset"
      ; cmd =
          PromptThen
            ( "Filter using revset"
            , fun revset ->
                Fun
                  (fun () ->
                    if revset = ""
                    then Vars.ui_state.revset $= None
                    else Vars.ui_state.revset $= Some revset) )
      }
    ]
  ;;

  (*TODO:make a custom widget the renders the commit with and without selection.
    with selection replace the dot with a blue version and slightly blue tint the background *)
  let graph_view ~sw () =
    (*We have a seperate error var here instead of using a result type. This allows us to avoid using Lwd.bind which would cause our list selection to get reset anytime the content changes *)
    let error_var = Lwd.var None in
    let revset_ui =
      let$* rev_set = Vars.ui_state.revset |> Lwd.get |>$ Option.map W.string in
      rev_set
      |> Option.map (fun x ->
        x |> Ui.resize ~mw:10000 ~sw:1 |> Lwd.pure |> W.Box.box ~pad_w:0 ~pad_h:0)
      |> Option.value ~default:(Ui.empty |> Lwd.pure)
    in
    let items =
      let$ graph, rev_ids =
        (*TODO I think this ads a slight delay to everything becasue it makes things need to be renedered twice. maybe I could try getting rid of it*)
        Vars.ui_state.trigger_update
        |> Lwd.get
        |> Lwd.map2 (Lwd.get Vars.ui_state.revset) ~f:(fun revset _ ->
          try
            let res = graph_and_revs ?revset () in
            error_var $= None;
            res
          with
          | Jj_process.JJError (cmd, error) ->
            (*If we have an error generating the graph,likely because the revset is wrong,just show the errror*)
            error_var $= Some (error |> Jj_tui.AnsiReverse.colored_string |> Ui.atom);
            [||], [||])
      in
      (*We will make two arrays, one with both selectable and filler and one with only selectable*)
      let selectable_idx = ref 0 in
      let selectable_items = Array.make (Array.length graph) (Obj.magic ()) in
      let items =
        graph
        |> Array.map (fun x ->
          match x with
          | `Selectable x ->
            let ui =
              W.Lists.selectable_item
                (x ^ "\n"
                 (* TODO This won't work if we are on a branch, because that puts the @ further out*)
                 |> Jj_tui.AnsiReverse.colored_string
                 |> Ui.atom)
            in
            let data = W.Lists.{ ui; data = rev_ids.(!selectable_idx) } in
            (*Add to our selectable array*)
            Array.set selectable_items !selectable_idx data;
            selectable_idx := !selectable_idx + 1;
            W.Lists.(Selectable data)
          | `Filler x ->
            W.Lists.(
              Filler
                (" " ^ x ^ "\n"
                 |> Jj_tui.AnsiReverse.colored_string
                 |> Ui.atom
                 |> Lwd.pure)))
      in
      items
    in
    (* run commands when there is keybaord input*)
    let handleKeys = function
      | `ASCII k, [] ->
        handleInputs command_mapping k
      | _ ->
        `Unhandled
    in
    let list_ui =
      items
      |> W.Lists.selection_list_exclusions
           ~on_selection_change:(fun revision ->
             Eio.Fiber.fork ~sw @@ fun _ ->
             Vars.update_ui_state @@ fun _ ->
             Lwd.set Vars.ui_state.selected_revision revision;
             Global_funcs.update_views ())
           ~custom_handler:(fun _ key -> handleKeys key)
    in
    let final_ui =
      let$ list_ui = list_ui
      and$ error = Lwd.get error_var in
      match error with Some e -> e |> Ui.keyboard_area handleKeys | None -> list_ui
    in
    W.vbox [ revset_ui; final_ui ]
  ;;
end
