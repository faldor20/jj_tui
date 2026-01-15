open Jj_json

let%expect_test "parse_valid_jsonl" =
  let input =
    {|{"commit_id":"abc123","parents":[],"change_id":"xyz","description":"First commit","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"xy","change_id_rest":"z","commit_id_prefix":"abc","commit_id_rest":"123"}
{"commit_id":"def456","parents":["abc123"],"change_id":"uvw","description":"Second commit","working_copy":true,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":["main"],"author":{"email":"test@example.com","timestamp":"2024-01-02"},"change_id_prefix":"uv","change_id_rest":"w","commit_id_prefix":"def","commit_id_rest":"456"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     Printf.printf "Parsed %d commits\n" (List.length commits);
     List.iter
       (fun (c : jj_commit) ->
          Printf.printf
            "Commit: %s, Parents: [%s]\n"
            c.commit_id
            (String.concat ";" c.parents))
       commits
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Parsed 2 commits
    Commit: abc123, Parents: []
    Commit: def456, Parents: [abc123]
  |}]
;;

let%expect_test "parse_root_commit" =
  let input =
    {|{"commit_id":"root","parents":[],"change_id":"xyz","description":"Root","working_copy":false,"immutable":true,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"xy","change_id_rest":"z","commit_id_prefix":"ro","commit_id_rest":"ot"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let (c : jj_commit) = List.hd commits in
     Printf.printf "Root commit: %s, Parents: %d\n" c.commit_id (List.length c.parents)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Root commit: root, Parents: 0
  |}]
;;

let%expect_test "commits_to_nodes_parent_linking" =
  let input =
    {|{"commit_id":"parent","parents":[],"change_id":"p","description":"Parent","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"p","change_id_rest":"","commit_id_prefix":"par","commit_id_rest":"ent"}
{"commit_id":"child","parents":["parent"],"change_id":"c","description":"Child","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-02"},"change_id_prefix":"c","change_id_rest":"","commit_id_prefix":"chi","commit_id_rest":"ld"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let nodes = commits_to_nodes commits in
     Printf.printf "Converted %d nodes\n" (List.length nodes);
     nodes
     |> List.iter (fun (n : Render_jj_graph.node) ->
       Printf.printf "Node: %s, Parents: %d\n" n.commit_id (List.length n.parents));
     let child_node = List.nth nodes 1 in
     let parent_node = List.nth nodes 0 in
     let child_parent = List.hd child_node.parents in
     Printf.printf "Parent reference correct: %b\n" (child_parent == parent_node)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Converted 2 nodes
    Node: parent, Parents: 0
    Node: child, Parents: 1
    Parent reference correct: false
    |}]
;;

let%expect_test "parse_multiple_parents" =
  let input =
    {|{"commit_id":"parent1","parents":[],"change_id":"p1","description":"Parent 1","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"p","change_id_rest":"1","commit_id_prefix":"par","commit_id_rest":"ent1"}
{"commit_id":"parent2","parents":[],"change_id":"p2","description":"Parent 2","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-02"},"change_id_prefix":"p","change_id_rest":"2","commit_id_prefix":"par","commit_id_rest":"ent2"}
{"commit_id":"merge","parents":["parent1","parent2"],"change_id":"m","description":"Merge commit","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-03"},"change_id_prefix":"m","change_id_rest":"","commit_id_prefix":"mer","commit_id_rest":"ge"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let nodes = commits_to_nodes commits in
     let (merge_node : Render_jj_graph.node) = List.nth nodes 2 in
     Printf.printf
       "Merge node: %s, Parents: %d\n"
       merge_node.commit_id
       (List.length merge_node.parents);
     let parent1_node = List.nth nodes 0 in
     let parent2_node = List.nth nodes 1 in
     let merge_parent1 = List.nth merge_node.parents 0 in
     let merge_parent2 = List.nth merge_node.parents 1 in
     Printf.printf "First parent correct: %b\n" (merge_parent1 == parent1_node);
     Printf.printf "Second parent correct: %b\n" (merge_parent2 == parent2_node)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Merge node: merge, Parents: 2
    First parent correct: false
    Second parent correct: false
    |}]
;;

let%expect_test "parse_commit_with_bookmarks" =
  let input =
    {|{"commit_id":"abc123","parents":[],"change_id":"xyz","description":"Commit with bookmarks","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":["main","feature"],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"xy","change_id_rest":"z","commit_id_prefix":"abc","commit_id_rest":"123"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let (c : jj_commit) = List.hd commits in
     Printf.printf
       "Commit: %s, Bookmarks: [%s]\n"
       c.commit_id
       (String.concat ";" c.bookmarks)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Commit: abc123, Bookmarks: [main;feature]
  |}]
;;

let%expect_test "parse_wip_commit" =
  let input =
    {|{"commit_id":"wip123","parents":[],"change_id":"xyz","description":"wip: work in progress","working_copy":true,"immutable":false,"wip":true,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"xy","change_id_rest":"z","commit_id_prefix":"wip","commit_id_rest":"123"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let (c : jj_commit) = List.hd commits in
     Printf.printf
       "Commit: %s, WIP: %b, Working copy: %b\n"
       c.commit_id
       c.wip
       c.working_copy
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Commit: wip123, WIP: true, Working copy: true
  |}]
;;

let%expect_test "parse_invalid_json" =
  let input = {|{"commit_id":"abc123","parents":[]|} in
  (match parse_jj_log_output input with
   | Ok _ ->
     Printf.printf "Unexpected success\n"
   | Error msg ->
     Printf.printf "Error occurred: %b\n" (String.length msg > 0));
  [%expect
    {|
    Error occurred: true
  |}]
;;

let%expect_test "parse_empty_input" =
  let input = "" in
  (match parse_jj_log_output input with
   | Ok commits ->
     Printf.printf "Parsed %d commits\n" (List.length commits)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Parsed 0 commits
  |}]
;;

let%expect_test "commits_to_nodes_preserves_order" =
  let input =
    {|{"commit_id":"first","parents":[],"change_id":"f","description":"First","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"f","change_id_rest":"","commit_id_prefix":"fir","commit_id_rest":"st"}
{"commit_id":"second","parents":["first"],"change_id":"s","description":"Second","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-02"},"change_id_prefix":"s","change_id_rest":"","commit_id_prefix":"sec","commit_id_rest":"ond"}
{"commit_id":"third","parents":["second"],"change_id":"t","description":"Third","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-03"},"change_id_prefix":"t","change_id_rest":"","commit_id_prefix":"thi","commit_id_rest":"rd"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let nodes = commits_to_nodes commits in
     Printf.printf "Order preserved: ";
     nodes
     |> List.iter (fun (n : Render_jj_graph.node) -> Printf.printf "%s " n.commit_id);
     Printf.printf "\n"
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Order preserved: first second third
  |}]
;;

let%expect_test "commits_to_nodes_copies_fields" =
  let input =
    {|{"commit_id":"test","parents":[],"change_id":"xyz","description":"Test commit","working_copy":true,"immutable":true,"wip":true,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-01"},"change_id_prefix":"xy","change_id_rest":"z","commit_id_prefix":"te","commit_id_rest":"st"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let nodes = commits_to_nodes commits in
     let (n : Render_jj_graph.node) = List.hd nodes in
     Printf.printf
       "Node fields - commit_id: %s, change_id: %s, working_copy: %b, immutable: %b, \
        wip: %b\n"
       n.commit_id
       n.change_id
       n.working_copy
       n.immutable
       n.wip
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Node fields - commit_id: test, change_id: xyz, working_copy: true, immutable: true, wip: true
  |}]
;;

let%expect_test "commits_to_nodes_missing_parent_creates_elided" =
  let input =
    {|{"commit_id":"child","parents":["missing_parent"],"change_id":"c","description":"Child with missing parent","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-02"},"change_id_prefix":"c","change_id_rest":"","commit_id_prefix":"chi","commit_id_rest":"ld"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let nodes = commits_to_nodes commits in
     Printf.printf "Total nodes: %d\n" (List.length nodes);
     let child_node = List.nth nodes 0 in
     Printf.printf
       "Child node: %s, Parents: %d\n"
       child_node.commit_id
       (List.length child_node.parents);
     let parent = List.hd child_node.parents in
     Printf.printf "Parent is elided: %b\n" (Render_jj_graph.is_elided parent);
     Printf.printf "Parent commit_id: %s\n" parent.commit_id
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Total nodes: 1
    Child node: child, Parents: 1
    Parent is elided: true
    Parent commit_id: ~ELIDED~
  |}]
;;

let%expect_test "commits_to_nodes_multiple_children_same_missing_parent" =
  let input =
    {|{"commit_id":"child1","parents":["missing_parent"],"change_id":"c1","description":"Child 1","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-02"},"change_id_prefix":"c","change_id_rest":"1","commit_id_prefix":"chi","commit_id_rest":"ld1"}
{"commit_id":"child2","parents":["missing_parent"],"change_id":"c2","description":"Child 2","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-03"},"change_id_prefix":"c","change_id_rest":"2","commit_id_prefix":"chi","commit_id_rest":"ld2"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let nodes = commits_to_nodes commits in
     Printf.printf "Total nodes: %d\n" (List.length nodes);
     let child1 = List.nth nodes 0 in
     let child2 = List.nth nodes 1 in
     let parent1 = List.hd child1.parents in
     let parent2 = List.hd child2.parents in
     Printf.printf
       "Both parents are elided: %b\n"
       (Render_jj_graph.is_elided parent1 && Render_jj_graph.is_elided parent2);
     Printf.printf "Same parent object (physical equality): %b\n" (parent1 == parent2)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Total nodes: 2
    Both parents are elided: true
    Same parent object (physical equality): true
  |}]
;;

let%expect_test "commits_to_nodes_same_missing_parent_physical_equality" =
  let input =
    {|{"commit_id":"child1","parents":["missing_parent"],"change_id":"c1","description":"Child 1","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-02"},"change_id_prefix":"c","change_id_rest":"1","commit_id_prefix":"chi","commit_id_rest":"ld1"}
{"commit_id":"child2","parents":["missing_parent"],"change_id":"c2","description":"Child 2","working_copy":false,"immutable":false,"wip":false,"hidden":false,"divergent":false,"empty":false,"bookmarks":[],"author":{"email":"test@example.com","timestamp":"2024-01-03"},"change_id_prefix":"c","change_id_rest":"2","commit_id_prefix":"chi","commit_id_rest":"ld2"}|}
  in
  (match parse_jj_log_output input with
   | Ok commits ->
     let nodes = commits_to_nodes commits in
     Printf.printf "Converted %d nodes\n" (List.length nodes);
     let child1 = List.nth nodes 0 in
     let child2 = List.nth nodes 1 in
     let parent1 = List.hd child1.parents in
     let parent2 = List.hd child2.parents in
     Printf.printf
       "Both parents are elided: %b\n"
       (Render_jj_graph.is_elided parent1 && Render_jj_graph.is_elided parent2);
     Printf.printf "Same parent object (physical equality): %b\n" (parent1 == parent2)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect
    {|
    Converted 2 nodes
    Both parents are elided: true
    Same parent object (physical equality): true
    |}]
;;
