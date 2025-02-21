open Jj_tui.Logging

module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Notty
  open Jj_tui
  open Nottui
  open! Jj_tui.Util
  open Jj_commands.Make (Vars)
  open Jj_widgets.Make (Vars)
  module Process = Jj_process.Make (Vars)
  open Process
  open Jj_tui.Process_wrappers.Make (Process)

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

  let rec make_command_mapping (key_map : Key_map.graph_keys) : 'acommand list =
    [
      {
        key = key_map.show_help
      ; description = "Show help"
      ; cmd =
          Fun
            (fun _ ->
              ui_state.show_popup
              $= Some
                   (commands_list_ui ~include_arrows:true (get_command_mapping ()), "Help");
              ui_state.input $= `Mode (fun _ -> `Unhandled))
      }
    ; {
        key = key_map.prev
      ; description = "Move the working copy to the previous child "
      ; cmd = Cmd [ "prev" ]
      }
    ; {
        key = key_map.new_child.menu
      ; description = "Make a new change"
      ; cmd =
          SubCmd
            [
              {
                key = key_map.new_child.base
              ; cmd = Cmd_with_revs (Active [ "new" ])
              ; description = "Make new child commit"
              }
            ; {
                key = key_map.new_child.no_edit
              ; cmd = Cmd_with_revs (Active [ "new"; "--no-edit" ])
              ; description = "Same as 'new', but without editing the new commit"
              }
            ; {
                key = key_map.new_child.inline
              ; description = "Make a new change and insert it after the selected rev"
              ; cmd =
                  Dynamic
                    (fun () ->
                      Cmd ([ "new"; "--insert-after" ] @ Vars.get_active_revs ()))
              }
            ; {
                key = key_map.new_child.inline_no_edit
              ; description = "Same as 'new insert', but without editing the new commit"
              ; cmd =
                  Dynamic
                    (fun () ->
                      Cmd
                        ([ "new"; "--no-edit"; "--insert-after" ]
                         @ Vars.get_active_revs ()))
              }
            ]
      }
    ; {
        key = key_map.duplicate
      ; description = "Duplicate the current selected commits "
      ; cmd = Dynamic (fun () -> Cmd ([ "duplicate" ] @ Vars.get_active_revs ()))
      }
    ; {
        key = key_map.undo
      ; description = "Undo the last operation"
      ; cmd = Cmd [ "undo" ]
      }
    ; {
        key = key_map.commit.menu
      ; description = "Commit"
      ; cmd =
          SubCmd
            [
              {
                key = key_map.commit.base
              ; description =
                  "Describe this change and start working on a new rev (same as \
                   `describe` then `new`)"
              ; cmd =
                  PromptThen ("commit msg", fun msg -> Fun (fun () -> custom_commit msg))
              }
            ; {
                key = key_map.commit.no_edit
              ; description = "Same as commit but without editing the new commit"
              ; cmd =
                  PromptThen
                    ( "commit msg"
                    , fun msg -> Fun (fun () -> custom_commit ~edit:false msg) )
              }
            ]
      }
    ; {
        key = key_map.split
      ; description = "Split the current commit interacively"
      ; cmd = Dynamic_r (fun rev -> Cmd_I [ "split"; "-r"; rev; "-i" ])
      }
    ; {
        key = key_map.squash.menu
      ; description = "Squash/unsquash"
      ; cmd =
          SubCmd
            [
              {
                key = key_map.squash.into_parent
              ; description = "Squash into parent"
              ; cmd =
                  Fun
                    (fun _ ->
                      let rev = Vars.get_hovered_rev () in
                      let source_msg, dest_msg = get_messages rev (rev ^ "-") in
                      let new_msg =
                        [ dest_msg; source_msg ] |> String.concat_non_empty "\n"
                      in
                      jj [ "squash"; "--quiet"; "-r"; rev; "-m"; new_msg ] |> ignore)
              }
            ; {
                key = key_map.squash.into_rev
              ; description = "Squash into any commit"
              ; cmd =
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
                              ]) )
              }
            ; {
                key = key_map.squash.unsquash
              ; cmd = Dynamic_r (fun rev -> Cmd_I [ "unsquash"; "-r"; rev; "-i" ])
              ; description = "Interactivaly unsquash"
              }
            ; {
                key = key_map.squash.interactive_parent
              ; description = "Interactively choose what to squash into parent"
              ; cmd = Dynamic_r (fun rev -> Cmd_I [ "squash"; "-r"; rev; "-i" ])
              }
            ; {
                key = key_map.squash.interactive_rev
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
        key = key_map.edit
      ; cmd = Dynamic_r (fun rev -> Cmd [ "edit"; rev ])
      ; description = "Edit the selected revision"
      }
    ; {
        key = key_map.describe
      ; cmd =
          Dynamic_r (fun rev -> Prompt ("description", [ "describe"; "-r"; rev; "-m" ]))
      ; description = "Describe this revision"
      }
    ; {
        key = key_map.describe_editor
      ; cmd = Dynamic_r (fun rev -> Cmd_I [ "describe"; "-r"; rev ])
      ; description = "Describe this revision using an editor"
      }
    ; {
        key = key_map.resolve
      ; cmd = Dynamic_r (fun rev -> Cmd_I [ "resolve"; "-r"; rev ])
      ; description = "Resolve conflicts at this revision"
      }
    ; {
        key = key_map.rebase.menu
      ; description = "Rebase revision "
      ; cmd =
          SubCmd
            [
              {
                key = key_map.rebase.single
              ; description = "Rebase single revision "
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      Prompt ("Dest rev for " ^ rev, [ "rebase"; "-r"; rev; "-d" ]))
              }
            ; {
                key = key_map.rebase.with_descendants
              ; description = "Rebase revision and its decendents"
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      Prompt
                        ( Printf.sprintf "Dest rev for %s and it's decendents" rev
                        , [ "rebase"; "-s"; rev; "-d" ] ))
              }
            ; {
                key = key_map.rebase.with_bookmark
              ; description = "Rebase revision and all other revissions on its bookmark"
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      Prompt
                        ( "Dest rev for bookmark including " ^ rev
                        , [ "rebase"; "-b"; rev; "-d" ] ))
              }
            ]
      }
    ; {
        key = key_map.git.menu
      ; description = "Git commands"
      ; cmd =
          SubCmd
            [
              {
                key = key_map.git.push
              ; description = "git push"
              ; cmd =
                  Fun
                    (fun _ ->
                      let revs = Vars.get_active_revs () in
                      let subcmds =
                        [
                          {
                            key = key_map.git.push
                          ; description = "proceed"
                          ; cmd = Cmd ([ "git"; "push"; "--allow-new" ] @ (revs|>List.concat_map(fun x-> ["-r";x])))
                          }
                        ; {
                            key = key_map.git.fetch
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
                        jj_no_log
                          ~get_stderr:true
                          ([ "git"; "push"; "--allow-new"; "--dry-run";  ] @ (revs|>List.concat_map(fun x-> ["-r";x])))
                        |> AnsiReverse.colored_string
                        |> Ui.atom
                        |> Lwd.pure
                      in
                      let ui = W.vbox [ log; commands_list_ui subcmds ] in
                      ui_state.show_popup $= Some (ui, "Git push will:");
                      ui_state.input $= `Mode (command_input ~is_sub:true subcmds))
              }
            ; {
                key = key_map.git.fetch
              ; description = "git fetch"
              ; cmd = Cmd [ "git"; "fetch" ]
              }
            ]
      }
    ; {
        key = key_map.parallelize
      ; description =
          "Parallelize commits. Takes 2 commits and makes them have the\n\
           same parent and child. Run `jj parallelize` --help for details"
      ; cmd =
          PromptThen
            ( "list commits to parallelize"
            , fun x -> Cmd ([ "paralellize" ] @ (x |> String.split_on_char ' ')) )
      }
    ; {
        key = key_map.abandon
      ; description = "Abandon this change(removes just this change and rebases parents)"
      ; cmd =
          Dynamic
            (fun () ->
              let revs = Vars.get_active_revs () in
              Cmd ([ "abandon" ] @ revs)
              |> confirm_prompt ("abandon the revisions:\n" ^ (revs |> String.concat "\n")))
      }
    ; {
        key = key_map.bookmark.menu
      ; description = "Bookmark commands"
      ; cmd =
          SubCmd
            [
              {
                key = key_map.bookmark.create
              ; description = "Create new bookmark"
              ; cmd =
                  PromptThen
                    ( "Bookmark name to create"
                    , fun x ->
                        Cmd_r
                          ([ "bookmark"; "create" ]
                           @ [ x |> String.map (fun c -> if c = ' ' then '_' else c) ]) )
              }
            ; {
                key = key_map.bookmark.delete
              ; description = "Delete bookmark"
              ; cmd =
                  bookmark_select_prompt
                    branches_no_remote
                    "Bookmark to delete"
                    (fun bookmark ->
                       Cmd [ "bookmark"; "delete"; bookmark ]
                       |> confirm_prompt
                            (Printf.sprintf
                               "delete the bookmark: '%s' This will also delete it on \
                                the remote next \"git push\"."
                               bookmark))
              }
            ; {
                key = key_map.bookmark.forget
              ; description = "Forget bookmark"
              ; cmd =
                  bookmark_select_prompt
                    branches_no_remote
                    "Bookmark to forget"
                    (fun bookmark ->
                       Cmd [ "bookmark"; "forget"; bookmark ]
                       |> confirm_prompt
                            (Printf.sprintf
                               "forget the bookmark: '%s' . This will not delete it on \
                                the remote."
                               bookmark))
              }
            ; {
                key = key_map.bookmark.rename
              ; description = "Rename bookmark"
              ; cmd =
                  bookmark_select_prompt
                    branches_no_remote
                    "Select the bookmark to rename (only local/tracked bookmarks are \
                     shown)"
                    (fun curr_name ->
                       Prompt ("New bookmark name", [ "bookmark"; "rename"; curr_name ]))
              }
            ; {
                key = key_map.bookmark.set
              ; description = "Set bookmark to this change"
              ; cmd =
                  Dynamic_r
                    (fun rev ->
                      bookmark_select_prompt
                        branches_no_remote
                        ("Select the bookmark to set to rev: " ^ rev)
                        (fun bookmark ->
                           Cmd
                             [
                               "bookmark"; "set"; "-r"; get_hovered_rev (); "-B"; bookmark
                             ]))
              }
            ; {
                key = key_map.bookmark.track
              ; description = "track given remote bookmark"
              ; cmd =
                  bookmark_select_prompt
                    branches_remotes_not_tracked
                    "Select the bookmark to begin tracking"
                    (fun bookmark -> Cmd [ "bookmark"; "track"; bookmark ])
              }
            ; {
                key = key_map.bookmark.untrack
              ; description = "untrack given remote bookmark"
              ; cmd =
                  bookmark_select_prompt
                    branches_remotes_tracked
                    "Select the bookmark to untrack"
                    (fun bookmark -> Cmd [ "bookmark"; "untrack"; bookmark ])
              }
            ]
      }
    ; {
        key = key_map.filter
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

  and command_mapping = ref None

  and get_command_mapping () =
    match !command_mapping with
    | Some mapping ->
      mapping
    | None ->
      let mapping = make_command_mapping (Lwd.peek ui_state.config).key_map.graph in
      command_mapping := Some mapping;
      mapping
  ;;

  (*TODO:make a custom widget the renders the commit with and without selection.
    with selection replace the dot with a blue version and slightly blue tint the background *)
  let graph_view ~focus summary_focus () =
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
            let id = rev_ids.(!selectable_idx) in
            let data =
              W.Lists.
                {
                  ui
                ; id = id |> Global_vars.get_unique_id |> String.hash
                ; data = rev_ids.(!selectable_idx)
                }
            in
            (*Add to our selectable array*)
            Array.set selectable_items !selectable_idx data;
            selectable_idx := !selectable_idx + 1;
            W.Lists.(Selectable data)
          | `Filler x ->
            W.Lists.(
              Filler (" " ^ x |> Jj_tui.AnsiReverse.colored_string |> Ui.atom |> Lwd.pure)))
      in
      items
    in
    (* run commands when there is keybaord input*)
    let handleKeys = function
      | `Enter, [] ->
        Focus.request_reversable summary_focus;
        `Handled
      | k ->
        handleInputs (get_command_mapping ()) k
      | _ ->
        `Unhandled
    in
    let list_ui =
      items
      |> W.Lists.multi_selection_list_exclusions
           ~reset_selections:Vars.ui_state.reset_selection
           ~on_selection_change:(fun ~hovered ~selected ->
             (*Respond to change in selected revision*)
             Lwd.set Vars.ui_state.hovered_revision hovered;
             Lwd.set Vars.ui_state.selected_revisions selected;
             (*If the files are focused we shouldn't send this*)
             (if Focus.peek_has_focus focus
              then Show_view.(push_status (Graph_preview (Vars.get_hovered_rev ()))));
             [%log debug "Hovered revision: '%s'" (Global_vars.get_unique_id hovered)];
             Global_funcs.update_views_async ())
           ~custom_handler:(fun ~selected ~selectable_items key -> handleKeys key)
    in
    let final_ui =
      let$ list_ui = list_ui
      and$ _ =
        Focus.status focus |>$ fun focus ->
        if Focus.has_focus focus
        then Show_view.(push_status (Graph_preview (Vars.get_hovered_rev ())))
      and$ error = Lwd.get error_var in
      match error with Some e -> e |> Ui.keyboard_area handleKeys | None -> list_ui
    in
    W.vbox [ revset_ui; final_ui ]
  ;;
end
