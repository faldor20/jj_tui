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

  (* Import graph commands *)
  module GraphCommands = Graph_commands.Make (Vars)

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

  (* Remove the hardcoded make_command_mapping function and use the dynamic one *)
  let command_mapping = ref None

  let rec get_command_mapping () =
    match !command_mapping with
    | Some mapping ->
      mapping
    | None ->
      let key_map = (Lwd.peek ui_state.config).key_map.graph in
      let registry = GraphCommands.get_command_registry get_command_mapping in
      let mapping = build_command_keymap key_map registry in
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
            let max_commits = (Vars.config |> Lwd.peek).max_commits in
            let res = graph_and_revs ?revset max_commits () in
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
                (x 
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
              Filler (x |> Jj_tui.AnsiReverse.colored_string |> Ui.atom |> Lwd.pure)))
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
