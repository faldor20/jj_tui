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

  (** Render commit content for a node - shows change_id, author, timestamp, description, bookmarks *)
  let render_commit_content (node : Render_jj_graph.node) : Notty.image =
    let open Notty in
    let open Notty.A in
    let styled_text attr text = I.string attr text in
    let change_id_short =
      String.sub node.change_id 0 (min 8 (String.length node.change_id))
    in
    let author_name =
      match String.split_on_char '@' node.author_email with
      | name :: _ ->
        name
      | [] ->
        node.author_email
    in
    let description_line =
      match String.split_on_char '\n' node.description with
      | first :: _ when String.trim first <> "" ->
        String.trim first
      | _ ->
        "(no description set)"
    in
    let parts = ref [] in
    let change_id_attr =
      if node.is_preview
      then fg lightblack ++ st dim
      else if node.working_copy
      then fg lightcyan ++ st bold
      else if node.immutable
      then fg lightmagenta
      else if node.empty
      then fg yellow
      else fg cyan
    in
    parts := styled_text change_id_attr change_id_short :: !parts;
    parts := styled_text (fg white ++ st dim) (" " ^ author_name) :: !parts;
    parts := styled_text (fg white ++ st dim) (" " ^ node.author_timestamp) :: !parts;
    if List.length node.bookmarks > 0
    then (
      let bookmarks_str = " (" ^ String.concat ", " node.bookmarks ^ ")" in
      parts := styled_text (fg green ++ st bold) bookmarks_str :: !parts);
    let desc_attr =
      if node.is_preview || node.empty
      then fg white ++ st dim
      else if node.wip
      then fg lightyellow
      else fg white
    in
    parts := styled_text desc_attr (" " ^ description_line) :: !parts;
    !parts |> List.rev |> I.hcat
  ;;

  (** Render a graph row by combining graph prefix with content *)
  let render_graph_row
        (row : Render_jj_graph.graph_row_output)
        ~(render_content : Render_jj_graph.node -> Notty.image) : Notty.image
    =
    let open Notty in
    let graph_img = I.string A.empty row.graph_chars in
    match row.row_type with
    | NodeRow ->
      let content_img = render_content row.node in
      I.hcat [ graph_img; content_img ]
    | LinkRow | PadRow | TermRow ->
      graph_img
  ;;

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
      let$ rendered_rows, rev_ids =
        (*TODO I think this ads a slight delay to everything becasue it makes things need to be renedered twice. maybe I could try getting rid of it*)
        Vars.ui_state.trigger_update
        |> Lwd.get
        |> Lwd.map2 (Lwd.get Vars.ui_state.revset) ~f:(fun revset _ ->
          try
            let max_commits = (Vars.config |> Lwd.peek).max_commits in
            let nodes, rev_ids = get_graph_nodes ?revset max_commits in
            let state =
              Render_jj_graph.{ depth = 0; columns = [||]; pending_joins = [] }
            in
            let rendered_rows = Render_jj_graph.render_nodes_structured state nodes in
            error_var $= None;
            rendered_rows, rev_ids
          with
          | Jj_process.JJError (cmd, error) ->
            (*If we have an error generating the graph,likely because the revset is wrong,just show the errror*)
            error_var $= Some (error |> Jj_tui.AnsiReverse.colored_string |> Ui.atom);
            [], [||])
      in
      (*We will make two arrays, one with both selectable and filler and one with only selectable*)
      let selectable_idx = ref 0 in
      let selectable_items = Array.make (Array.length rev_ids) (Obj.magic ()) in
      let items =
        rendered_rows
        |> List.map (fun (row : Render_jj_graph.graph_row_output) ->
          match row.row_type with
          | NodeRow ->
            let ui =
              W.Lists.selectable_item
                (render_graph_row row ~render_content:render_commit_content |> Ui.atom)
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
          | LinkRow | PadRow | TermRow ->
            let graph_img = I.string A.empty row.graph_chars in
            W.Lists.(Filler (graph_img |> Ui.atom |> Lwd.pure)))
        |> Array.of_list
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
