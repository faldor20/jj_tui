(**
   `jj_json.ml`

   Module for parsing jj log JSON output and converting to render_jj_graph nodes.
   Provides types and functions to:
   - Define the jj template for JSON output
   - Parse JSONL (JSON Lines) output from jj log
   - Convert parsed commits to render_jj_graph.node list
*)

(** Author information from jj log output *)
type jj_author = {
    email : string
  ; timestamp : string
}
[@@deriving yojson]

(** Commit information from jj log JSON output *)
type jj_commit = {
    commit_id : string
  ; parents : string list
  ; change_id : string
  ; description : string
  ; working_copy : bool
  ; immutable : bool
  ; trunk : bool
  ; wip : bool
  ; hidden : bool
  ; divergent : bool
  ; conflict : bool
  ; empty : bool
  ; local_bookmarks : string list
  ; remote_bookmarks : string list
  ; tags : string list
  ; author : jj_author
  ; change_id_prefix : string
  ; change_id_rest : string
  ; commit_id_prefix : string
  ; commit_id_rest : string
}
[@@deriving yojson]

(** The jj template that produces JSONL output *)
let json_log_template =
  {|'{'
  ++ '"commit_id":' ++ json(commit_id)
  ++ ',"parents":[' ++ parents.map(|c| json(c.commit_id())).join(",") ++ ']'
  ++ ',"change_id":' ++ json(change_id)
  ++ ',"description":' ++ json(description)
  ++ ',"working_copy":' ++ json(current_working_copy)
  ++ ',"immutable":' ++ json(immutable)
  ++ ',"trunk":' ++ json(self.contained_in("trunk()"))
  ++ ',"wip":' ++ json(description.first_line().starts_with("wip:"))
  ++ ',"hidden":' ++ json(hidden)
  ++ ',"divergent":' ++ json(divergent)
  ++ ',"conflict":' ++ json(conflict)
  ++ ',"empty":' ++ json(empty)
  ++ ',"local_bookmarks":['
  ++ local_bookmarks
       .map(|b| json(stringify(if(!b.synced(), b.name() ++ "*", b.name()))))
       .join(",")
  ++ ']'
  ++ ',"remote_bookmarks":['
  ++ remote_bookmarks
       .map(|b| json(stringify(b.name() ++ "@" ++ b.remote())))
       .join(",")
  ++ ']'
  ++ ',"tags":['
  ++ tags.map(|t| json(t.name())).join(",")
  ++ ']'
  ++ ',"author":{"email":' ++ json(author.email().local()) ++ ',"timestamp":' ++ json(author.timestamp().local().format("%Y-%m-%d %H:%M:%S")) ++ '}'
  ++ ',"change_id_prefix":' ++ json(change_id.shortest(8).prefix())
  ++ ',"change_id_rest":' ++ json(change_id.shortest(8).rest())
  ++ ',"commit_id_prefix":' ++ json(commit_id.shortest(8).prefix())
  ++ ',"commit_id_rest":' ++ json(commit_id.shortest(8).rest())
  ++ '}
'|}
;;

(** Parse JSONL (one JSON object per line) from jj log output.
    When graph is included, trim all content before the first '{' on each line
    and skip lines without '{' (graph-only lines). *)
let parse_jj_log_output (input : string) : (jj_commit list, string) result =
  try
    let ensure_bool_field field_name default = function
      | `Assoc fields as json ->
        if List.mem_assoc field_name fields
        then json
        else `Assoc ((field_name, `Bool default) :: fields)
      | json ->
        json
    in
    let lines =
      input |> String.split_on_char '\n' |> List.filter (fun s -> String.length s > 0)
    in
    let commits =
      lines
      |> List.filter_map (fun line ->
        (* Find the first '{' to skip graph characters *)
        match String.index_opt line '{' with
        | None ->
          (* No JSON on this line, skip it (e.g., graph-only lines) *)
          None
        | Some idx ->
          (* Extract JSON from first '{' to end of line *)
          let json_str = String.sub line idx (String.length line - idx) in
          Some json_str)
      |> List.map (fun json_str ->
        let json = Yojson.Safe.from_string json_str in
        (* Older tests and fixtures may not include newly added template fields.
            Normalize them here so the parser stays backward-compatible. *)
        let json = json |> ensure_bool_field "trunk" false in
        match jj_commit_of_yojson json with
        | Ok commit ->
          commit
        | Error msg ->
          failwith (Printf.sprintf "Failed to parse commit JSON: %s" msg))
    in
    Ok commits
  with
  | Failure msg ->
    Error (Printf.sprintf "Parse error: %s" msg)
  | Yojson.Json_error msg ->
    Error (Printf.sprintf "JSON error: %s" msg)
  | ex ->
    Error (Printf.sprintf "Unexpected error: %s" (Printexc.to_string ex))
;;

(** Convert list of jj_commit to render_jj_graph.node list.

    The renderer tracks columns by physical node identity, so child parent pointers
    must resolve to the exact finalized parent objects that also appear in the main
    node list. We therefore build one shared DAG for the full commit set first, then
    optionally derive a second emitted graph where non-visible ancestry is
    collapsed into synthetic elision nodes with the correct visible parents. *)
let commits_to_nodes
      ?visible_commit_ids
      ?(collapse_hidden_ancestry = true)
      (commits : jj_commit list) : Render_jj_graph.node list
  =
  let display_refs (jj_commit : jj_commit) =
    (* Local bookmarks occupy the visible commit row; otherwise keep the remote-only
       label. Tags are appended after refs to match jj's short header output. *)
    let primary_refs =
      if jj_commit.local_bookmarks <> []
      then jj_commit.local_bookmarks
      else jj_commit.remote_bookmarks
    in
    primary_refs @ jj_commit.tags
  in
  (* Index raw commits so the recursive builder can materialize the final node DAG
     in one pass instead of replacing placeholder nodes later. *)
  let commit_tbl : (string, jj_commit) Hashtbl.t = Hashtbl.create (List.length commits) in
  commits
  |> List.iter (fun jj_commit -> Hashtbl.replace commit_tbl jj_commit.commit_id jj_commit);
  let full_node_tbl : (string, Render_jj_graph.node) Hashtbl.t =
    Hashtbl.create (List.length commits)
  in
  let anonymous_elided_tbl : (string, Render_jj_graph.node) Hashtbl.t =
    Hashtbl.create 8
  in
  let rec build_full_node commit_id =
    match Hashtbl.find_opt full_node_tbl commit_id with
    | Some node ->
      node
    | None ->
      (match Hashtbl.find_opt commit_tbl commit_id with
       | None ->
         (match Hashtbl.find_opt anonymous_elided_tbl commit_id with
          | Some elided ->
            elided
          | None ->
            let elided = Render_jj_graph.make_elided_node ~id:commit_id () in
            Hashtbl.add anonymous_elided_tbl commit_id elided;
            elided)
       | Some jj_commit ->
         let parents = List.map build_full_node jj_commit.parents in
         let node : Render_jj_graph.node =
           {
             parents
           ; creation_time = Int64.of_int 0
           ; working_copy = jj_commit.working_copy
           ; immutable = jj_commit.immutable
           ; wip = jj_commit.wip
           ; change_id = jj_commit.change_id
           ; commit_id = jj_commit.commit_id
           ; description = jj_commit.description
           ; bookmarks = display_refs jj_commit
           ; author_email = jj_commit.author.email
           ; author_timestamp = jj_commit.author.timestamp
           ; empty = jj_commit.empty
           ; hidden = jj_commit.hidden
           ; divergent = jj_commit.divergent
           ; conflict = jj_commit.conflict
           ; is_preview = false
           ; change_id_prefix = jj_commit.change_id_prefix
           ; change_id_rest = jj_commit.change_id_rest
           ; commit_id_prefix = jj_commit.commit_id_prefix
           ; commit_id_rest = jj_commit.commit_id_rest
           }
         in
         Hashtbl.add full_node_tbl commit_id node;
         node)
  in
  List.iter (fun jj_commit -> ignore (build_full_node jj_commit.commit_id)) commits;
  if not collapse_hidden_ancestry
  then (
    let visible_commit_ids =
      match visible_commit_ids with
      | Some ids ->
        ids
      | None ->
        commits |> List.map (fun commit -> commit.commit_id)
    in
    visible_commit_ids
    |> List.filter_map (fun commit_id -> Hashtbl.find_opt full_node_tbl commit_id))
  else (
    let visible_commit_ids =
      match visible_commit_ids with
      | Some ids ->
        ids
      | None ->
        commits |> List.map (fun commit -> commit.commit_id)
    in
    let visible_set =
      visible_commit_ids |> List.to_seq |> Seq.map (fun id -> id, ()) |> Hashtbl.of_seq
    in
    let rec visible_parent_ids commit_id =
      match Hashtbl.find_opt commit_tbl commit_id with
      | None ->
        [ commit_id ]
      | Some jj_commit ->
        jj_commit.parents
        |> List.concat_map (fun parent_id ->
          if Hashtbl.mem visible_set parent_id || not (Hashtbl.mem commit_tbl parent_id)
          then [ parent_id ]
          else visible_parent_ids parent_id)
    in
    let collapsed_node_tbl : (string, Render_jj_graph.node) Hashtbl.t =
      Hashtbl.create (List.length visible_commit_ids * 2)
    in
    let rec build_collapsed_node node_id =
      match Hashtbl.find_opt collapsed_node_tbl node_id with
      | Some node ->
        node
      | None ->
        if Render_jj_graph.is_elided_id node_id
        then failwith "build_collapsed_node should not be called directly for elided ids"
        else (
          let jj_commit = Hashtbl.find commit_tbl node_id in
          let parents =
            jj_commit.parents
            |> List.concat_map (fun parent_id ->
              if Hashtbl.mem visible_set parent_id && Hashtbl.mem commit_tbl parent_id
              then [ build_collapsed_node parent_id ]
              else if
                Hashtbl.mem commit_tbl parent_id || not (Hashtbl.mem commit_tbl parent_id)
              then [ build_elided_node ~child_id:node_id ~hidden_parent_id:parent_id ]
              else [])
          in
          let node : Render_jj_graph.node =
            {
              parents
            ; creation_time = Int64.of_int 0
            ; working_copy = jj_commit.working_copy
            ; immutable = jj_commit.immutable
            ; wip = jj_commit.wip
            ; change_id = jj_commit.change_id
            ; commit_id = jj_commit.commit_id
            ; description = jj_commit.description
            ; bookmarks = display_refs jj_commit
            ; author_email = jj_commit.author.email
            ; author_timestamp = jj_commit.author.timestamp
            ; empty = jj_commit.empty
            ; hidden = jj_commit.hidden
            ; divergent = jj_commit.divergent
            ; conflict = jj_commit.conflict
            ; is_preview = false
            ; change_id_prefix = jj_commit.change_id_prefix
            ; change_id_rest = jj_commit.change_id_rest
            ; commit_id_prefix = jj_commit.commit_id_prefix
            ; commit_id_rest = jj_commit.commit_id_rest
            }
          in
          Hashtbl.add collapsed_node_tbl node_id node;
          node)
    and build_elided_node ~child_id ~hidden_parent_id =
      let elided_id =
        Printf.sprintf "%s:%s:%s" Render_jj_graph.elided_marker child_id hidden_parent_id
      in
      match Hashtbl.find_opt collapsed_node_tbl elided_id with
      | Some node ->
        node
      | None ->
        let parents =
          visible_parent_ids hidden_parent_id
          |> List.map (fun parent_id ->
            if Hashtbl.mem commit_tbl parent_id && Hashtbl.mem visible_set parent_id
            then build_collapsed_node parent_id
            else build_full_node parent_id)
        in
        let elided = Render_jj_graph.make_elided_node ~id:elided_id ~parents () in
        Hashtbl.add collapsed_node_tbl elided_id elided;
        elided
    in
    let emitted = Hashtbl.create (List.length visible_commit_ids * 2) in
    commits
    |> List.concat_map (fun jj_commit ->
      if not (Hashtbl.mem visible_set jj_commit.commit_id)
      then []
      else (
        let node = build_collapsed_node jj_commit.commit_id in
        let elided_nodes =
          jj_commit.parents
          |> List.filter (fun parent_id -> not (Hashtbl.mem visible_set parent_id))
          |> List.map (fun parent_id ->
            build_elided_node ~child_id:jj_commit.commit_id ~hidden_parent_id:parent_id)
        in
        node :: elided_nodes))
    |> List.filter (fun (node : Render_jj_graph.node) ->
      if Hashtbl.mem emitted node.commit_id
      then false
      else (
        Hashtbl.add emitted node.commit_id ();
        true)))
;;

(** Select the commits that should remain visible before the renderer collapses the
    rest of the ancestry into synthetic elision rows. The graph still comes from
    `all()`, but the UI revset can further narrow which of these retained commits
    are shown. *)
let select_visible_commit_ids ?filter_commit_ids (commits : jj_commit list) : string list =
  (* Only local bookmarks pin immutable commits open in the default graph. Remote-only
     refs are informational and should not prevent ancestry elision. *)
  let has_branch_assignment (commit : jj_commit) = commit.local_bookmarks <> [] in
  let retained_commit_ids : (string, unit) Hashtbl.t =
    Hashtbl.create (List.length commits * 2)
  in
  let retain commit_id = Hashtbl.replace retained_commit_ids commit_id () in
  commits
  |> List.iter (fun (commit : jj_commit) ->
    if not commit.immutable
    then (
      retain commit.commit_id;
      commit.parents |> List.iter retain)
    (* we don't need to include parents of branch commits*)
    else if commit.trunk || has_branch_assignment commit
    then retain commit.commit_id);
  let filter_set =
    match filter_commit_ids with
    | None ->
      None
    | Some commit_ids ->
      Some (commit_ids |> List.to_seq |> Seq.map (fun id -> id, ()) |> Hashtbl.of_seq)
  in
  commits
  |> List.filter_map (fun (commit : jj_commit) ->
    if not (Hashtbl.mem retained_commit_ids commit.commit_id)
    then None
    else (
      match filter_set with
      | None ->
        Some commit.commit_id
      | Some filter_set ->
        if Hashtbl.mem filter_set commit.commit_id then Some commit.commit_id else None))
;;
