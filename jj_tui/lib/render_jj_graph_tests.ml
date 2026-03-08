open Render_jj_graph

let make_node ?(parents = []) commit_id : node =
  {
    parents
  ; creation_time = 0L
  ; working_copy = false
  ; immutable = false
  ; wip = false
  ; change_id = commit_id
  ; commit_id
  ; description = commit_id
  ; bookmarks = []
  ; author_email = ""
  ; author_timestamp = ""
  ; empty = false
  ; hidden = false
  ; divergent = false
  ; conflict = false
  ; is_preview = false
  ; change_id_prefix = ""
  ; change_id_rest = ""
  ; commit_id_prefix = ""
  ; commit_id_rest = ""
  }
;;

let find_node_exn nodes commit_id = nodes |> List.find (fun n -> n.commit_id = commit_id)
let find_preview_exn nodes = nodes |> List.find (fun n -> n.is_preview)
let parent_ids node = node.parents |> List.map (fun p -> p.commit_id)

let%expect_test "apply_rebase_preview_insert_before" =
  let c = make_node "c" in
  let b = make_node ~parents:[ c ] "b" in
  let a = make_node ~parents:[ b ] "a" in
  let nodes, invalid =
    apply_rebase_preview
      ~mode:`Insert_before
      ~sources:[ "c" ]
      ~targets:[ "b" ]
      [ a; b; c ]
  in
  let preview = find_preview_exn nodes in
  let b = find_node_exn nodes "b" in
  print_endline (Option.value invalid ~default:"ok");
  parent_ids preview |> String.concat "," |> print_endline;
  parent_ids b |> String.concat "," |> print_endline;
  [%expect
    {|
    ok

    preview:preview:b
    |}]
;;

let%expect_test "apply_rebase_preview_insert_after" =
  let c = make_node "c" in
  let b = make_node ~parents:[ c ] "b" in
  let a = make_node ~parents:[ b ] "a" in
  let nodes, invalid =
    apply_rebase_preview ~mode:`Insert_after ~sources:[ "a" ] ~targets:[ "b" ] [ a; b; c ]
  in
  let preview = find_preview_exn nodes in
  let has_a = nodes |> List.exists (fun n -> n.commit_id = "a") in
  print_endline (Option.value invalid ~default:"ok");
  parent_ids preview |> String.concat "," |> print_endline;
  print_endline (string_of_bool has_a);
  [%expect
    {|
    ok
    b
    false
    |}]
;;

let%expect_test "apply_rebase_preview_removes_sources" =
  let c = make_node "c" in
  let b = make_node ~parents:[ c ] "b" in
  let a = make_node ~parents:[ b ] "a" in
  let nodes, _invalid =
    apply_rebase_preview ~mode:`Add_after ~sources:[ "a" ] ~targets:[ "b" ] [ a; b; c ]
  in
  let ids = nodes |> List.map (fun n -> n.commit_id) |> String.concat "," in
  print_endline ids;
  [%expect {| preview:preview:b,b,c |}]
;;

let%expect_test "apply_rebase_preview_invalid_cycle" =
  let c = make_node "c" in
  let b = make_node ~parents:[ c ] "b" in
  let a = make_node ~parents:[ b ] "a" in
  let nodes, invalid =
    apply_rebase_preview
      ~mode:`Insert_before
      ~sources:[ "a" ]
      ~targets:[ "b" ]
      [ a; b; c ]
  in
  let preview_count = nodes |> List.filter (fun n -> n.is_preview) |> List.length in
  print_endline (Option.value invalid ~default:"ok");
  print_endline (string_of_int preview_count);
  [%expect
    {|
    Preview blocked: cycle detected
    0
    |}]
;;

let%expect_test "recreate_target_graph" =
  (* Graph: a (working copy) has parents [c; d] where c->b->d
     First parent (c) gets node's column (0), second parent (d) branches to column 1.
  *)
  let d : node =
    {
      parents = []
    ; creation_time = 4L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let b : node =
    {
      parents = [ d ]
    ; creation_time = 3L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let c : node =
    {
      parents = [ b ]
    ; creation_time = 2L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let a : node =
    {
      parents = [ c; d ]
    ; creation_time = 1L
    ; working_copy = true
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let state : state = { depth = 0; columns = [||]; pending_joins = [] } in
  render_nodes_to_string state [ a; c; b; d ] |> print_endline;
  [%expect
    {|
    @
    ├─╮
    ○ │
    ○ │
    ├─╯
    ○
    |}]
;;

let%expect_test "complex_graph_golden_graph_only" =
  (* This test encodes the graph shape from the user-provided example.
     We only assert the graph glyphs (left side), not the metadata text.

     Topology (using the change ids from the example):
     - @ (wwtl...) has parents: yrsq..., otsz..., loxn...
     - ○ (xysm...) has parents: yrsq..., otsz..., loxn...
     - loxn..., otsz..., yrsq..., qlnop..., osynn... all have parent rzmu...
     - tzrqs... has parent osynn...
     - rzmu... has parent zzz...
     - zzz... is immutable (◆)
  *)
  let zzz : node =
    {
      parents = []
    ; creation_time = 0L
    ; working_copy = false
    ; immutable = true
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let rzmu : node =
    {
      parents = [ zzz ]
    ; creation_time = 1L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let osynn : node =
    {
      parents = [ rzmu ]
    ; creation_time = 2L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let tzrqs : node =
    {
      parents = [ osynn ]
    ; creation_time = 3L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let qlnop : node =
    {
      parents = [ rzmu ]
    ; creation_time = 4L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let loxn : node =
    {
      parents = [ rzmu ]
    ; creation_time = 5L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let otsz : node =
    {
      parents = [ rzmu ]
    ; creation_time = 6L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let yrsq : node =
    {
      parents = [ rzmu ]
    ; creation_time = 7L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let xysm : node =
    {
      parents = [ yrsq; otsz; loxn ]
    ; creation_time = 8L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let wwtl : node =
    {
      parents = [ yrsq; otsz; loxn ]
    ; creation_time = 9L
    ; working_copy = true
    ; immutable = false
    ; wip = false
    ; change_id = ""
    ; commit_id = ""
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  (* Render order matching the example top-to-bottom. *)
  let state : state = { depth = 0; columns = [||]; pending_joins = [] } in
  let info_rows (n : node) = if n == loxn || n == tzrqs || n == rzmu then 1 else 0 in
  render_nodes_to_string
    ~info_rows
    state
    [ wwtl; xysm; loxn; otsz; yrsq; qlnop; tzrqs; osynn; rzmu; zzz ]
  |> print_endline;
  [%expect
    {|
    @
    ├─┬─╮
    │ │ │ ○
    ╭─┬─┬─╯
    │ │ ○
    │ │ │
    │ ○ │
    │ ├─╯
    ○ │
    ├─╯
    │ ○
    ├─╯
    │ ○
    │ │
    │ ○
    ├─╯
    ○
    │
    ◆
    |}]
;;

type jj_output_author = {
    name : string
  ; email : string
  ; timestamp : string
}
[@@deriving yojson]

type jj_output = {
    commit_id : string
  ; parents : string list
  ; change_id : string
  ; description : string
  ; working_copy : bool
  ; immutable : bool
  ; wip : bool
  ; author : jj_output_author
  ; committer : jj_output_author
}
[@@deriving yojson]

let%expect_test "render_jj_output" =
  let read_file path =
    let ic = Stdlib.open_in_bin path in
    Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () ->
         let len = Stdlib.in_channel_length ic in
         Stdlib.really_input_string ic len)
  in
  let source_root = Sys.getenv_opt "DUNE_SOURCEROOT" |> Option.value ~default:"." in
  let file_input = read_file (Filename.concat source_root "test/jj_log.json") in
  let raw_nodes =
    file_input
    |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length s > 0)
    |> List.map (fun x ->
      x |> Yojson.Safe.from_string |> jj_output_of_yojson |> Result.get_ok)
  in
  (* First pass: create all nodes without parents and populate hashtable *)
  let node_tbl : (string, node) Hashtbl.t = Hashtbl.create (List.length raw_nodes) in
  raw_nodes
  |> List.iter (fun jj_node ->
    let n : node =
      {
        parents = [] (* populated in second pass *)
      ; creation_time = Int64.of_int 0
      ; working_copy = jj_node.working_copy
      ; immutable = jj_node.immutable
      ; wip = jj_node.wip
      ; change_id = jj_node.change_id
      ; commit_id = jj_node.commit_id
      ; description = jj_node.description
      ; bookmarks = []
      ; author_email = jj_node.author.email
      ; author_timestamp = jj_node.author.timestamp
      ; empty = false
      ; hidden = false
      ; divergent = false
      ; conflict = false
      ; is_preview = false
      ; change_id_prefix = ""
      ; change_id_rest = ""
      ; commit_id_prefix = ""
      ; commit_id_rest = ""
      }
    in
    Hashtbl.add node_tbl jj_node.commit_id n);
  (* Second pass: link up parents in reverse order (so parents are resolved before children).
     We process in reverse so that when we look up a parent, it's already been updated with
     its own parents. Then we update the hashtable with the complete node. *)
  let rev_raw_nodes = List.rev raw_nodes in
  rev_raw_nodes
  |> List.iter (fun jj_node ->
    let parents =
      jj_node.parents
      |> List.map (fun parent_id ->
        match Hashtbl.find_opt node_tbl parent_id with
        | Some p ->
          p
        | None ->
          failwith
            (Printf.sprintf
               "Parent %s not found for node %s (change_id=%s)"
               parent_id
               jj_node.commit_id
               jj_node.change_id))
    in
    (* Verify we didn't drop any parents *)
    if List.length parents <> List.length jj_node.parents
    then
      failwith
        (Printf.sprintf
           "Parent count mismatch for node %s: expected %d, got %d"
           jj_node.commit_id
           (List.length jj_node.parents)
           (List.length parents));
    let node = Hashtbl.find node_tbl jj_node.commit_id in
    let updated_node = { node with parents } in
    Hashtbl.replace node_tbl jj_node.commit_id updated_node);
  (* Extract nodes in original order, now with proper parent links *)
  let processed_nodes =
    raw_nodes |> List.map (fun jj_node -> Hashtbl.find node_tbl jj_node.commit_id)
  in
  let state : state = { depth = 0; columns = [||]; pending_joins = [] } in
  render_nodes_to_string state processed_nodes |> print_endline;
  [%expect
    {|
    ○
    │ ○
    ╭─┼─┬─╮
    │ │ │ │ ○
    │ │ ╭───┤
    │ │ │ │ ○
    │ │ │ │ ○
    │ │ │ │ ○
    │ ├─────╯
    │ ○ │ │
    ╭─┴─╮ │
    │   ○ │
    │   ├─╯
    @   │
    ├───╯
    ○
    ├─╮
    │ │ ○
    │ │ ○
    ╭─┬─╯
    │ ○
    │ ◌
    │ ○
    │ ├─┬─╮
    │ │ │ ○
    ├─────╯
    │ │ ○
    ├───╯
    │ ○
    ├─╯
    │ ○
    ├─╯
    │ ○
    ├─╯
    ○
    ◆
    |}]
;;

let%expect_test "make_elided_node" =
  let elided = Render_jj_graph.make_elided_node () in
  Printf.printf "commit_id: %s\n" elided.commit_id;
  Printf.printf "change_id: %s\n" elided.change_id;
  Printf.printf "description: %s\n" elided.description;
  Printf.printf "hidden: %b\n" elided.hidden;
  [%expect
    {|
    commit_id: ~ELIDED~
    change_id: ~ELIDED~
    description: (elided revisions)
    hidden: true
    |}]
;;

let%expect_test "is_elided_true" =
  let elided = Render_jj_graph.make_elided_node () in
  Printf.printf "is_elided: %b\n" (Render_jj_graph.is_elided elided);
  [%expect
    {|
    is_elided: true
    |}]
;;

let%expect_test "is_elided_false" =
  let normal =
    {
      parents = []
    ; creation_time = Int64.zero
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = "abc"
    ; commit_id = "123"
    ; description = "test"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  Printf.printf "is_elided: %b\n" (Render_jj_graph.is_elided normal);
  [%expect
    {|
    is_elided: false
    |}]
;;

let%expect_test "render_nodes_structured_simple" =
  let parent : node =
    {
      parents = []
    ; creation_time = Int64.zero
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = "p"
    ; commit_id = "parent"
    ; description = "Parent"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let child : node =
    {
      parent with
      parents = [ parent ]
    ; commit_id = "child"
    ; change_id = "c"
    ; description = "Child"
    }
  in
  let state : state = { depth = 0; columns = [||]; pending_joins = [] } in
  let rows =
    Render_jj_graph.render_nodes_structured state [ child; parent ] ~info_lines:(fun _ ->
      1)
  in
  Printf.printf "Total rows: %d\n" (List.length rows);
  List.iteri
    (fun i row ->
       let row_type_str =
         match row.row_type with
         | NodeRow ->
           "NodeRow"
         | LinkRow ->
           "LinkRow"
         | PadRow ->
           "PadRow"
         | TermRow ->
           "TermRow"
       in
       Printf.printf
         "Row %d: %s | node=%s | graph='%s'\n"
         i
         row_type_str
         row.node.commit_id
         row.graph_chars)
    rows;
  [%expect
    {|
    Total rows: 4
    Row 0: NodeRow | node=child | graph='○'
    Row 1: PadRow | node=child | graph='│'
    Row 2: NodeRow | node=parent | graph='○'
    Row 3: PadRow | node=parent | graph=''
    |}]
;;

let%expect_test "render_nodes_structured_row_types" =
  let d : node =
    {
      parents = []
    ; creation_time = 4L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = "d"
    ; commit_id = "d"
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let b : node =
    {
      parents = [ d ]
    ; creation_time = 3L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = "b"
    ; commit_id = "b"
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let c : node =
    {
      parents = [ b ]
    ; creation_time = 2L
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = "c"
    ; commit_id = "c"
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let a : node =
    {
      parents = [ c; d ]
    ; creation_time = 1L
    ; working_copy = true
    ; immutable = false
    ; wip = false
    ; change_id = "a"
    ; commit_id = "a"
    ; description = "test commit"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01T00:00:00Z"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let state : state = { depth = 0; columns = [||]; pending_joins = [] } in
  let rows =
    Render_jj_graph.render_nodes_structured state [ a; c; b; d ] ~info_lines:(fun _ -> 0)
  in
  Printf.printf "Total rows: %d\n" (List.length rows);
  let node_rows = List.filter (fun r -> r.row_type = NodeRow) rows in
  let link_rows = List.filter (fun r -> r.row_type = LinkRow) rows in
  let pad_rows = List.filter (fun r -> r.row_type = PadRow) rows in
  Printf.printf
    "NodeRows: %d, LinkRows: %d, PadRows: %d\n"
    (List.length node_rows)
    (List.length link_rows)
    (List.length pad_rows);
  List.iter
    (fun row ->
       let row_type_str =
         match row.row_type with
         | NodeRow ->
           "NodeRow"
         | LinkRow ->
           "LinkRow"
         | PadRow ->
           "PadRow"
         | TermRow ->
           "TermRow"
       in
       Printf.printf "%s: '%s'\n" row_type_str row.graph_chars)
    rows;
  [%expect
    {|
    Total rows: 6
    NodeRows: 4, LinkRows: 2, PadRows: 0
    NodeRow: '@'
    LinkRow: '├─╮'
    NodeRow: '○ │'
    NodeRow: '○ │'
    LinkRow: '├─╯'
    NodeRow: '○'
    |}]
;;

let%expect_test "elided_parent_creates_termination_line" =
  let elided = Render_jj_graph.make_elided_node () in
  let child : node =
    {
      parents = [ elided ]
    ; creation_time = Int64.zero
    ; working_copy = false
    ; immutable = false
    ; wip = false
    ; change_id = "c"
    ; commit_id = "child"
    ; description = "Child with elided parent"
    ; bookmarks = []
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01"
    ; empty = false
    ; hidden = false
    ; divergent = false
    ; conflict = false
    ; is_preview = false
    ; change_id_prefix = ""
    ; change_id_rest = ""
    ; commit_id_prefix = ""
    ; commit_id_rest = ""
    }
  in
  let state : state = { depth = 0; columns = [||]; pending_joins = [] } in
  let rows =
    Render_jj_graph.render_nodes_structured state [ child ] ~info_lines:(fun _ -> 1)
  in
  Printf.printf "Total rows: %d\n" (List.length rows);
  List.iteri
    (fun i row ->
       let row_type_str =
         match row.row_type with
         | NodeRow ->
           "NodeRow"
         | LinkRow ->
           "LinkRow"
         | PadRow ->
           "PadRow"
         | TermRow ->
           "TermRow"
       in
       Printf.printf
         "Row %d: %s | node=%s | graph='%s'\n"
         i
         row_type_str
         row.node.commit_id
         row.graph_chars)
    rows;
  [%expect
    {|
    Total rows: 5
    Row 0: NodeRow | node=child | graph='○'
    Row 1: PadRow | node=child | graph='│'
    Row 2: TermRow | node=child | graph='~'
    Row 3: PadRow | node=child | graph=''
    Row 4: PadRow | node=child | graph=''
    |}]
;;
