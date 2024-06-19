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
        key = '5'
      ; description = "Show help2"
      ; cmd =
          SubCmd
            [
              {
                key = '1'
              ; description = "Show help2"
              ; cmd =
                  Fun
                    (fun _ ->
                      ui_state.show_popup
                      $= Some (commands_list_ui command_mapping, "Help");
                      ui_state.input $= `Mode (fun _ -> `Unhandled))
              }
            ]
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
              { key = 'i'; description = "Make a new empty change"; cmd = Cmd [ "new" ] }
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
                      jj [ "squash"; "--quiet"; "-m"; new_msg ] |> ignore)
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
      ; cmd = Prompt ("revision", [ "edit" ])
      ; description = "Edit a particular revision"
      }
    ; {
        key = 'd'
      ; cmd = Prompt ("description", [ "describe"; "-m" ])
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
              ; description = "Rebase single revision"
              ; cmd =
                  Prompt ("destination for revision rebase", [ "rebase"; "-r"; "@"; "-d" ])
              }
            ; {
                key = 's'
              ; description = "Rebase revision and its decendents"
              ; cmd =
                  Prompt
                    ("Destination for decendent rebase", [ "rebase"; "-s"; "@"; "-d" ])
              }
            ; {
                key = 'b'
              ; description = "Rebase revision and all other revissions on its branch"
              ; cmd =
                  Prompt ("Destination for branch rebase", [ "rebase"; "-b"; "@"; "-d" ])
              }
            ]
      }
    ; {
        key = 'g'
      ; description = "Git commands"
      ; cmd =
          SubCmd
            [
              { key = 'p'; description = "git push branch"; cmd = Cmd [ "git"; "push" ] }
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
      ; cmd = Cmd [ "abandon" ] |> confirm_prompt "abandon the change"
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
              ; cmd = Prompt ("Branch to set to this commit ", [ "branch"; "set"; "-B" ])
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

  (*Renders the commit graph from the UI state*)
  let graph_view =
    Wd.scroll_area (ui_state.jj_tree $-> Ui.atom)
    |>$ Ui.keyboard_area (function
      | `ASCII k, [] ->
        handleInputs command_mapping k
      | _ ->
        `Unhandled)
  ;;
end
