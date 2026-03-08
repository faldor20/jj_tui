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

  (* Use the library's render function for commit content *)
  let render_commit_content = Commit_render.render_commit_content

  (** Group rows by their owning node. Each group is (node_row, continuation_rows).
      Each NodeRow starts a new group containing it and all following non-NodeRows until
      the next NodeRow. *)
  let group_rows_by_node (rows : Render_jj_graph.graph_row_output list) :
    (Render_jj_graph.graph_row_output * Render_jj_graph.graph_row_output list) list
    =
    let open Render_jj_graph in
    let rec loop acc current_group = function
      | [] ->
        List.rev (match current_group with Some g -> g :: acc | None -> acc)
      | row :: rest ->
        (match row.row_type with
         | NodeRow ->
           let acc = match current_group with Some g -> g :: acc | None -> acc in
           loop acc (Some (row, [])) rest
         | _ ->
           (match current_group with
            | Some (node_row, conts) ->
              loop acc (Some (node_row, conts @ [ row ])) rest
            | None ->
              (* Orphan row, shouldn't happen, but skip it *)
              loop acc None rest))
    in
    loop [] None rows
  ;;

  (** Render a node group by distributing content lines across available rows.
      Returns a list of (row, rendered_image) pairs. *)
  let render_node_group
        ((node_row, continuation_rows) :
          Render_jj_graph.graph_row_output * Render_jj_graph.graph_row_output list)
        ~(render_content : Render_jj_graph.node -> Notty.image list) :
    (Render_jj_graph.graph_row_output * Notty.image) list
    =
    let open Notty in
    let open Render_jj_graph in
    let content_lines = render_content node_row.node in
    let available_rows = node_row :: continuation_rows in
    let result = ref [] in
    (* Distribute content lines across available rows *)
    List.iteri
      (fun i row ->
         let graph_img = row.graph_image in
         let combined =
           if i < List.length content_lines
           then I.hcat [ graph_img; List.nth content_lines i ]
           else graph_img
         in
         result := (row, combined) :: !result)
      available_rows;
    (* If content needs more lines than available, add synthetic continuation rows *)
    if List.length content_lines > List.length available_rows
    then (
      let node_glyphs = [ "○"; "@"; "◌"; "◆"; "×" ] in
      let synthetic_graph =
        let chars = node_row.graph_chars in
        let replaced = ref chars in
        List.iter
          (fun glyph ->
             replaced := Str.global_replace (Str.regexp_string glyph) "│" !replaced)
          node_glyphs;
        I.string A.empty !replaced
      in
      for i = List.length available_rows to List.length content_lines - 1 do
        let line_img = List.nth content_lines i in
        result := (node_row, I.hcat [ synthetic_graph; line_img ]) :: !result
      done);
    List.rev !result
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
    let mode_indicator =
      let$ active = Lwd.get Vars.ui_state.rebase_preview_active
      and$ mode = Lwd.get Vars.ui_state.rebase_preview_mode
      and$ source_mode = Lwd.get Vars.ui_state.rebase_preview_source_mode
      and$ invalid = Lwd.get Vars.ui_state.rebase_preview_invalid in
      if not active
      then Ui.empty
      else (
        let mode_str =
          match mode with
          | `Insert_before ->
            "insert-before"
          | `Insert_after ->
            "insert-after"
          | `Add_after ->
            "add-after"
        in
        let source_str =
          match source_mode with
          | `Revisions ->
            "revisions"
          | `Source ->
            "source"
          | `Branch ->
            "branch"
        in
        let base = Printf.sprintf "Preview: dest=%s source=%s" mode_str source_str in
        let label = match invalid with None -> base | Some msg -> base ^ " - " ^ msg in
        W.string label)
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
            let node_id_map =
              List.map2
                (fun node rev_id -> node.Render_jj_graph.commit_id, rev_id)
                nodes
                (Array.to_list rev_ids)
              |> List.to_seq
              |> Hashtbl.of_seq
            in
            let nodes, invalid =
              if Vars.get_rebase_preview_active ()
              then (
                let expanded_sources =
                  Render_jj_graph.expand_preview_sources
                    ~mode:(Vars.get_rebase_preview_source_mode ())
                    ~sources:(Vars.get_rebase_preview_sources ())
                    ~targets:(Vars.get_rebase_preview_targets ())
                    nodes
                in
                Render_jj_graph.apply_rebase_preview
                  ~mode:(Vars.get_rebase_preview_mode ())
                  ~sources:expanded_sources
                  ~targets:(Vars.get_rebase_preview_targets ())
                  nodes)
              else nodes, None
            in
            let current_invalid = Lwd.peek Vars.ui_state.rebase_preview_invalid in
            if current_invalid <> invalid then Vars.set_rebase_preview_invalid invalid;
            let rev_ids =
              if Vars.get_rebase_preview_active ()
              then
                nodes
                |> List.filter (fun node -> not node.Render_jj_graph.is_preview)
                |> List.filter_map (fun node ->
                  Hashtbl.find_opt node_id_map node.Render_jj_graph.commit_id)
                |> Array.of_list
              else rev_ids
            in
            let rendered_rows =
              Render_jj_graph.render_nodes_structured
                state
                nodes
                ~node_attr:Commit_render.graph_node_attr
            in
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
      (* Group rows by node and render each group with content distribution *)
      let grouped_rows = group_rows_by_node rendered_rows in
      let items =
        grouped_rows
        |> List.concat_map (fun group ->
          let rendered_group =
            render_node_group group ~render_content:render_commit_content
          in
          (* Convert rendered group to list items: first is Selectable, rest are Fillers *)
          match rendered_group with
          | [] ->
            []
          | (first_row, first_img) :: rest_rows ->
            if first_row.node.is_preview
            then
              List.map
                (fun (_row, img) -> W.Lists.(Filler (img |> Ui.atom |> Lwd.pure)))
                ((first_row, first_img) :: rest_rows)
            else (
              let id = rev_ids.(!selectable_idx) in
              let selectable_ui = W.Lists.selectable_item (first_img |> Ui.atom) in
              let data =
                W.Lists.
                  {
                    ui = selectable_ui
                  ; id = id |> Global_vars.get_unique_id |> String.hash
                  ; data = rev_ids.(!selectable_idx)
                  }
              in
              (* Add to our selectable array *)
              Array.set selectable_items !selectable_idx data;
              selectable_idx := !selectable_idx + 1;
              let first_item = W.Lists.(Selectable data) in
              (* All other rows in the group become fillers *)
              let filler_items =
                List.map
                  (fun (_row, img) -> W.Lists.(Filler (img |> Ui.atom |> Lwd.pure)))
                  rest_rows
              in
              first_item :: filler_items))
        |> Array.of_list
      in
      items
    in
    (* run commands when there is keybaord input*)
    let handleKeys = function
      | `Escape, [] when Vars.get_rebase_preview_active () ->
        Vars.clear_rebase_preview ();
        Vars.ui_state.trigger_update $= ();
        Vars.ui_state.input $= `Normal;
        `Handled
      | `Enter, [] ->
        Focus.request_reversable summary_focus;
        `Handled
      | k ->
        if Vars.get_rebase_preview_active ()
        then `Unhandled
        else handleInputs (get_command_mapping ()) k
      | _ ->
        `Unhandled
    in
    let list_ui =
      items
      |> W.Lists.multi_selection_list_exclusions
           ~reset_selections:Vars.ui_state.reset_selection
           ~on_selection_change:(fun ~hovered ~selected ->
             let prev_hovered = Lwd.peek Vars.ui_state.hovered_revision in
             let prev_selected = Lwd.peek Vars.ui_state.selected_revisions in
             if prev_hovered <> hovered || prev_selected <> selected
             then (
               (*Respond to change in selected revision*)
               Lwd.set Vars.ui_state.hovered_revision hovered;
               Lwd.set Vars.ui_state.selected_revisions selected;
               if Vars.get_rebase_preview_active ()
               then (
                 let targets =
                   if List.length selected = 0
                   then [ hovered |> Global_vars.get_unique_id ]
                   else selected |> List.map Global_vars.get_unique_id
                 in
                 let current_targets = Vars.get_rebase_preview_targets () in
                 if targets <> current_targets
                 then (
                   Vars.set_rebase_preview_targets targets;
                   Vars.ui_state.trigger_update $= ()))
               else (
                 (*If the files are focused we shouldn't send this*)
                 (if Focus.peek_has_focus focus
                  then Show_view.(push_status (Graph_preview (Vars.get_hovered_rev ()))));
                 [%log debug "Hovered revision: '%s'" (Global_vars.get_unique_id hovered)];
                 Global_funcs.update_views_async ())))
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
    W.vbox [ revset_ui; mode_indicator; final_ui ]
  ;;
end
