open Notty

(** Group rows by their owning node, preserving graph-only rows that can appear
    before a visible node after elision. Native jj emits those rows before the node,
    so the layout layer needs to keep that ordering instead of forcing everything to
    trail the node row. *)

type node_group = {
    pre_rows : Render_jj_graph.graph_row_output list
  ; node_row : Render_jj_graph.graph_row_output
  ; continuation_rows : Render_jj_graph.graph_row_output list
}

let group_rows_by_node_raw (rows : Render_jj_graph.graph_row_output list) :
  node_group list
  =
  let open Render_jj_graph in
  let rec loop acc pending_rows current_group = function
    | [] ->
      let acc = match current_group with Some g -> g :: acc | None -> acc in
      List.rev acc
    | row :: rest ->
      (match row.row_type with
       | NodeRow ->
         let acc = match current_group with Some g -> g :: acc | None -> acc in
         loop
           acc
           []
           (Some { pre_rows = pending_rows; node_row = row; continuation_rows = [] })
           rest
       | _ ->
         (match current_group with
          | Some ({ continuation_rows; _ } as group) ->
            loop
              acc
              pending_rows
              (Some { group with continuation_rows = continuation_rows @ [ row ] })
              rest
          | None ->
            (* Native jj can emit graph-only rows like `│` or `~` before the next
               visible node after an elision gap. Keep them and attach them before
               that node instead of dropping or reordering them. *)
            loop acc (pending_rows @ [ row ]) None rest))
  in
  loop [] [] None rows
;;

let contains_str s substr =
  try
    let _ = Str.search_forward (Str.regexp_string substr) s 0 in
    true
  with
  | Not_found ->
    false
;;

let has_leading_branch_node (row : Render_jj_graph.graph_row_output) =
  (contains_str row.graph_chars "○"
   || contains_str row.graph_chars "@"
   || contains_str row.graph_chars "◌"
   || contains_str row.graph_chars "◆"
   || contains_str row.graph_chars "×")
  && contains_str row.graph_chars "│"
;;

let is_plain_vertical_pad (row : Render_jj_graph.graph_row_output) =
  row.row_type = Render_jj_graph.PadRow && String.trim row.graph_chars = "│"
;;

let is_branch_continuation_pad (row : Render_jj_graph.graph_row_output) =
  row.row_type = Render_jj_graph.PadRow && String.trim row.graph_chars = "│ │"
;;

let normalize_join_rows (groups : node_group list) : node_group list =
  let rec loop acc = function
    | ({ continuation_rows = prev_conts; _ } as prev_group)
      :: ({ node_row = next_node; continuation_rows = next_conts; _ } as next_group)
      :: rest ->
      (match List.rev prev_conts with
       | (trailing_link : Render_jj_graph.graph_row_output) :: prev_rev_rest
         when trailing_link.row_type = Render_jj_graph.LinkRow
              && has_leading_branch_node next_node
              && List.exists is_plain_vertical_pad next_conts
              && not (List.exists is_branch_continuation_pad next_conts) ->
         let prev_group =
           { prev_group with continuation_rows = List.rev prev_rev_rest }
         in
         let next_group =
           { next_group with continuation_rows = trailing_link :: next_conts }
         in
         loop (prev_group :: acc) (next_group :: rest)
       | _ ->
         loop (prev_group :: acc) (next_group :: rest))
    | [ group ] ->
      List.rev (group :: acc)
    | [] ->
      List.rev acc
  in
  loop [] groups
;;

let group_rows_by_node rows = rows |> group_rows_by_node_raw |> normalize_join_rows

let render_node_group
      ({ pre_rows; node_row; continuation_rows } : node_group)
      ~(render_content : Render_jj_graph.node -> Notty.image list) :
  (Render_jj_graph.graph_row_output * Notty.image) list
  =
  let content_lines = render_content node_row.node in
  let content_rows, trailing_graph_only_rows =
    let available_rows = node_row :: continuation_rows in
    available_rows
    |> List.partition (fun (row : Render_jj_graph.graph_row_output) ->
      row.row_type <> Render_jj_graph.TermRow)
  in
  let result = ref [] in
  (* Distribute content lines across node/link/pad rows only. Term rows such as
     `~ (elided revisions)` must remain graph-only so commit descriptions stay on
     a vertical continuation instead of being glued to the elision marker. *)
  List.iteri
    (fun i (row : Render_jj_graph.graph_row_output) ->
       let combined =
         if i < List.length content_lines
         then I.hcat [ row.graph_image; List.nth content_lines i ]
         else row.graph_image
       in
       result := (row, combined) :: !result)
    content_rows;
  (* When native jj doesn't provide enough content-bearing rows for a two-line
     commit, keep the description visually attached by replacing the node glyph
     with a vertical line before appending any graph-only term rows. *)
  if List.length content_lines > List.length content_rows
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
    for i = List.length content_rows to List.length content_lines - 1 do
      let line_img = List.nth content_lines i in
      result := (node_row, I.hcat [ synthetic_graph; line_img ]) :: !result
    done);
  pre_rows
  |> List.iter (fun (row : Render_jj_graph.graph_row_output) ->
    result := (row, row.graph_image) :: !result);
  trailing_graph_only_rows
  |> List.iter (fun (row : Render_jj_graph.graph_row_output) ->
    result := (row, row.graph_image) :: !result);
  let rendered_rows = List.rev !result in
  let is_node_row ((row, _img) : Render_jj_graph.graph_row_output * Notty.image) =
    row == node_row
  in
  match List.find_opt is_node_row rendered_rows with
  | None ->
    rendered_rows
  | Some node_entry ->
    let other_rows = List.filter (fun entry -> not (is_node_row entry)) rendered_rows in
    node_entry :: other_rows
;;
