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
    Uses two-pass approach: create nodes, then link parents.
    
    The two-pass algorithm is required because parent references must point to
    the exact same objects in memory (physical equality ==). This is achieved by:
    1. First pass: Create all nodes with empty parent lists, store in Hashtbl
    2. Second pass: Process in reverse order, look up parents from Hashtbl, update nodes
*)
let commits_to_nodes (commits : jj_commit list) : Render_jj_graph.node list =
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
  (* First pass: create all nodes without parents and populate hashtable *)
  let node_tbl : (string, Render_jj_graph.node) Hashtbl.t =
    Hashtbl.create (List.length commits)
  in
  commits
  |> List.iter (fun jj_commit ->
    let n : Render_jj_graph.node =
      {
        parents = [] (* populated in second pass *)
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
    Hashtbl.add node_tbl jj_commit.commit_id n);
  (* Second pass: link up parents in reverse order (so parents are resolved before children).
     We process in reverse so that when we look up a parent, it's already been updated with
     its own parents. Then we update the hashtable with the complete node.
     
     If a parent is missing (not in the fetched commit list), we create an elided node
     to represent it. This allows rendering graphs with incomplete history. *)
  let elided_nodes = ref [] in
  let rev_commits = List.rev commits in
  rev_commits
  |> List.iter (fun jj_commit ->
    let parents =
      jj_commit.parents
      |> List.map (fun parent_id ->
        match Hashtbl.find_opt node_tbl parent_id with
        | Some p ->
          p
        | None ->
          let elided = Render_jj_graph.make_elided_node () in
          Hashtbl.add node_tbl parent_id elided;
          elided_nodes := elided :: !elided_nodes;
          elided)
    in
    let node = Hashtbl.find node_tbl jj_commit.commit_id in
    let updated_node = { node with parents } in
    Hashtbl.replace node_tbl jj_commit.commit_id updated_node);
  let base_nodes =
    commits |> List.map (fun jj_commit -> Hashtbl.find node_tbl jj_commit.commit_id)
  in
  base_nodes
;;
