(**
   `commit_render.ml`

   Module for rendering commit nodes to Notty images.
   Handles rendering commit metadata with proper styling including shortest unique prefix highlighting.
*)

open Notty

(** Render an ID with prefix highlighting.
    The prefix gets the full color attribute, while the rest gets a dimmed version. *)
let render_id ~prefix_attr ~rest_attr ~prefix ~rest =
  if String.length rest > 0
  then I.(string prefix_attr prefix <|> string rest_attr rest)
  else I.string prefix_attr prefix
;;

(** Color for the graph node glyph based on node state. *)
let graph_node_attr (node : Render_jj_graph.node) : Notty.A.t =
  let open Notty.A in
  if node.is_preview
  then fg lightblack
  else if node.working_copy
  then fg green ++ st bold
  else if node.conflict
  then fg red ++ st bold
  else if node.divergent
  then fg red ++ st bold
  else if node.immutable
  then fg cyan
  else fg white
;;


(** The amount of padding to add to the left of the commit content. *)
let pad_amount = 2
let add_padding img = I.pad ~l:pad_amount img
(** Render commit content for a node - shows change_id, author, timestamp, description, bookmarks.
Matches original jj format:
Line 1: change_id email timestamp commit_id_short
Line 2: (empty) description
*)
let render_commit_content (node : Render_jj_graph.node) : Notty.image list =
  let open Notty in
  let open Notty.A in

  let magenta = if node.working_copy then lightmagenta else magenta in
  (* make style bold if working copy *)
  let bs = if node.working_copy then st bold else st A.no_style in
  let styled_text attr text = I.string attr text in
  let description_line =
    match String.split_on_char '\n' node.description with
    | first :: _ when String.trim first <> "" ->
      String.trim first
    | _ ->
      "(no description set)"
  in
  (* Line 1: change_id email timestamp bookmarks commit_id_short *)
  let line1_parts = ref [] in
  (* Render change_id with prefix highlighting *)
  let change_id_prefix_attr, change_id_rest_attr =
    if node.hidden
    then
      let duplicate_attr = fg white ++ st bold in
      duplicate_attr, duplicate_attr
    else if node.divergent
    then fg red ++ st bold, fg red ++ bs
    else fg magenta ++ st bold, fg lightblack ++ bs
  in
  let change_id_img =
    render_id
      ~prefix_attr:change_id_prefix_attr
      ~rest_attr:change_id_rest_attr
      ~prefix:node.change_id_prefix
      ~rest:node.change_id_rest
  in
  line1_parts := change_id_img :: !line1_parts;
  (* Author email and timestamp *)
  line1_parts := styled_text (fg yellow ++ bs) (" " ^ node.author_email) :: !line1_parts;
  line1_parts := styled_text (fg cyan ++ bs) (" " ^ node.author_timestamp) :: !line1_parts;
  (* Add bookmarks after timestamp if they exist *)
  if List.length node.bookmarks > 0
  then (
    let bookmarks_str = " " ^ String.concat " " node.bookmarks in
    line1_parts := styled_text (fg magenta) bookmarks_str :: !line1_parts);
  (* Render commit_id with prefix highlighting *)
  let commit_id_prefix_attr =
    (if node.working_copy then fg lightblue else fg blue) ++ st bold
  in
  let commit_id_rest_attr = fg lightblack ++ bs in
  let commit_id_img =
    render_id
      ~prefix_attr:commit_id_prefix_attr
      ~rest_attr:commit_id_rest_attr
      ~prefix:(" " ^ node.commit_id_prefix)
      ~rest:node.commit_id_rest
  in
  line1_parts := commit_id_img :: !line1_parts;
  let labels =
    [ if node.hidden then Some (fg lightblack ++ bs, "(hidden)") else None
    ; if node.divergent then Some (fg red ++ bs, "(divergent)") else None
    ]
    |> List.filter_map Fun.id
  in
  labels
  |> List.iter (fun (attr, label) ->
    line1_parts := styled_text attr (" " ^ label) :: !line1_parts);
  let line1 = !line1_parts |> List.rev |> I.hcat in
  (* Line 2: (empty) description *)
  let desc_attr =
    (if node.is_preview || node.empty
     then lightgreen
     else if node.description = ""
     then yellow
     else white)
    |> fg
    |> ( ++ ) bs
  in
  let description_with_prefix =
    if node.empty then "(empty) " ^ description_line else description_line
  in
  let line2 = styled_text desc_attr description_with_prefix in
  [ add_padding line1; add_padding line2 ]
;;
