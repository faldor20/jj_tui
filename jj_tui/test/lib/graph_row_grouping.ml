open Jj_tui

module Test_native_graph = Process_wrappers.Make (struct
    let jj_no_log ?get_stderr:_ ?snapshot:_ ?color:_ _ = failwith "unused"
  end)

(** These tests exercise the shared row-layout implementation used by graph_view so
    the post-elision branch regression is locked down end-to-end. *)

let test_node ?(description = "desc") commit_id =
  let open Render_jj_graph in
  {
    parents = []
  ; creation_time = Int64.zero
  ; working_copy = false
  ; immutable = false
  ; wip = false
  ; change_id = commit_id
  ; commit_id
  ; description
  ; bookmarks = []
  ; author_email = "test@example.com"
  ; author_timestamp = "2024-01-01"
  ; empty = false
  ; hidden = false
  ; divergent = false
  ; conflict = false
  ; is_preview = false
  ; change_id_prefix = commit_id
  ; change_id_rest = ""
  ; commit_id_prefix = "deadbeef"
  ; commit_id_rest = ""
  }
;;

let test_row ?(row_type = Render_jj_graph.PadRow) node graph_chars =
  Render_jj_graph.
    {
      graph_chars
    ; graph_image = Notty.I.string Notty.A.empty graph_chars
    ; node
    ; row_type
    }
;;

let render_content_lines node =
  [
    Notty.I.string
      Notty.A.empty
      (node.Render_jj_graph.change_id
       ^ " test@example.com 2024-01-01 "
       ^ node.commit_id_prefix)
  ; Notty.I.string Notty.A.empty node.Render_jj_graph.description
  ]
;;

let render_image_to_string img =
  let trim_right s =
    let rec last_non_space i =
      if i < 0
      then -1
      else if Char.equal s.[i] ' ' || Char.equal s.[i] '\n' || Char.equal s.[i] '\r'
      then last_non_space (i - 1)
      else i
    in
    let last = last_non_space (String.length s - 1) in
    if last < 0 then "" else String.sub s 0 (last + 1)
  in
  let buf = Buffer.create 256 in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (120, 1) img;
  Buffer.contents buf |> trim_right
;;

let print_group i (group : Graph_row_layout.node_group) =
  let node_row : Render_jj_graph.graph_row_output = group.node_row in
  let pre_rows : Render_jj_graph.graph_row_output list = group.pre_rows in
  let continuation_rows : Render_jj_graph.graph_row_output list =
    group.continuation_rows
  in
  Printf.printf "Group %d node=%s\n" i node_row.node.commit_id;
  pre_rows
  |> List.iter (fun (row : Render_jj_graph.graph_row_output) ->
    Printf.printf "  pre=%S\n" row.graph_chars);
  Printf.printf "  head=%S\n" node_row.graph_chars;
  continuation_rows
  |> List.iter (fun (row : Render_jj_graph.graph_row_output) ->
    Printf.printf "  cont=%S\n" row.graph_chars)
;;

let rows_with_join_before_child () =
  let top =
    test_node ~description:"ensure commit actually puts the rev in the right place" "top"
  in
  let child = test_node ~description:"show conflicts correctly" "child" in
  [
    test_row ~row_type:Render_jj_graph.NodeRow top "◆  "
  ; test_row ~row_type:Render_jj_graph.LinkRow top "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow child "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow child "│  "
  ]
;;

let rows_with_elided_termination_case () =
  let mmnxzuyv = test_node ~description:"show conflicts correctly" "mmnxzuyv" in
  let upnslvuv =
    test_node ~description:"make bookmarks render origin if need" "upnslvuv"
  in
  let upnslvuv_2 =
    test_node ~description:"make bookmarks render origin if needed" "upnslvuv/2"
  in
  let lpztppmx =
    test_node ~description:"fix elided revisions bug during rebase" "lpztppmx"
  in
  let vxkltmxw = test_node ~description:"(empty) (no description set)" "vxkltmxw" in
  let mwqvkttl = test_node ~description:"(empty) (no description set)" "mwqvkttl" in
  let qqsnmuzr_2 =
    test_node
      ~description:"fix preview mode bugs (I think this broke the update loop somehow)"
      "qqsnmuzr/2"
  in
  let tkozwuzw = test_node ~description:"enable preview mode (5.2 codex)" "tkozwuzw" in
  let nkwwwlnw = test_node ~description:"rewrite" "nkwwwlnw" in
  let voywlxnk = test_node ~description:"(no description set)" "voywlxnk" in
  let wrrtuusr = test_node ~description:"(no description set)" "wrrtuusr" in
  let vvnqynuv = test_node ~description:"(no description set)" "vvnqynuv" in
  let noszsqtm =
    test_node
      ~description:"remove aarch64 linux because it doesn't seem to work"
      "noszsqtm"
  in
  [
    test_row ~row_type:Render_jj_graph.NodeRow mmnxzuyv "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow mmnxzuyv "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow upnslvuv "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow upnslvuv "│  "
  ; test_row ~row_type:Render_jj_graph.NodeRow upnslvuv_2 "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow upnslvuv_2 "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow lpztppmx "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow lpztppmx "│  "
  ; test_row ~row_type:Render_jj_graph.TermRow lpztppmx "~  (elided revisions)"
  ; test_row ~row_type:Render_jj_graph.NodeRow vxkltmxw "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow vxkltmxw "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow mwqvkttl "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow mwqvkttl "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow qqsnmuzr_2 "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow qqsnmuzr_2 "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow tkozwuzw "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow tkozwuzw "│  "
  ; test_row ~row_type:Render_jj_graph.TermRow tkozwuzw "~  (elided revisions)"
  ; test_row ~row_type:Render_jj_graph.NodeRow nkwwwlnw "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow nkwwwlnw "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow voywlxnk "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow voywlxnk "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow wrrtuusr "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow wrrtuusr "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow vvnqynuv "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow vvnqynuv "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow noszsqtm "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow noszsqtm "│  "
  ; test_row ~row_type:Render_jj_graph.TermRow noszsqtm "~"
  ]
;;

let rows_with_synthetic_termination_case () =
  let mmnxzuyv = test_node ~description:"show conflicts correctly" "mmnxzuyv" in
  let upnslvuv =
    test_node ~description:"make bookmarks render origin if need" "upnslvuv"
  in
  let upnslvuv_2 =
    test_node ~description:"make bookmarks render origin if needed" "upnslvuv/2"
  in
  let lpztppmx =
    test_node ~description:"fix elided revisions bug during rebase" "lpztppmx"
  in
  let vxkltmxw = test_node ~description:"(empty) (no description set)" "vxkltmxw" in
  let mwqvkttl = test_node ~description:"(empty) (no description set)" "mwqvkttl" in
  [
    test_row ~row_type:Render_jj_graph.NodeRow mmnxzuyv "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow mmnxzuyv "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow upnslvuv "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow upnslvuv "│  "
  ; test_row ~row_type:Render_jj_graph.NodeRow upnslvuv_2 "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow upnslvuv_2 "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow lpztppmx "│ ◆  "
  ; test_row ~row_type:Render_jj_graph.TermRow lpztppmx "~  (elided revisions)"
  ; test_row ~row_type:Render_jj_graph.NodeRow vxkltmxw "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow vxkltmxw "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow mwqvkttl "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow mwqvkttl "├─╯  "
  ]
;;

let rows_with_extra_trailing_vertical_bug () =
  let lpztppmx =
    test_node ~description:"fix elided revisions bug during rebase" "lpztppmx"
  in
  let vxkltmxw = test_node ~description:"(empty) (no description set)" "vxkltmxw" in
  let mwqvkttl = test_node ~description:"(empty) (no description set)" "mwqvkttl" in
  let qqsnmuzr_2 =
    test_node
      ~description:"fix preview mode bugs (I think this broke the update loop somehow)"
      "qqsnmuzr/2"
  in
  let tkozwuzw = test_node ~description:"enable preview mode (5.2 codex)" "tkozwuzw" in
  let nkwwwlnw = test_node ~description:"rewrite" "nkwwwlnw" in
  let voywlxnk = test_node ~description:"(no description set)" "voywlxnk" in
  let wrrtuusr = test_node ~description:"(no description set)" "wrrtuusr" in
  let vvnqynuv = test_node ~description:"(no description set)" "vvnqynuv" in
  let noszsqtm =
    test_node
      ~description:"remove aarch64 linux because it doesn't seem to work"
      "noszsqtm"
  in
  [
    test_row ~row_type:Render_jj_graph.PadRow lpztppmx "│  "
  ; test_row ~row_type:Render_jj_graph.TermRow lpztppmx "~  (elided revisions)"
  ; test_row ~row_type:Render_jj_graph.NodeRow lpztppmx "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow lpztppmx "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow vxkltmxw "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow vxkltmxw "├─╯  "
  ; test_row ~row_type:Render_jj_graph.NodeRow mwqvkttl "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow mwqvkttl "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow qqsnmuzr_2 "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow qqsnmuzr_2 "├─╯  "
  ; test_row ~row_type:Render_jj_graph.PadRow tkozwuzw "│  "
  ; test_row ~row_type:Render_jj_graph.TermRow tkozwuzw "~  (elided revisions)"
  ; test_row ~row_type:Render_jj_graph.NodeRow tkozwuzw "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow tkozwuzw "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow nkwwwlnw "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow nkwwwlnw "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow voywlxnk "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow voywlxnk "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow wrrtuusr "│ ○  "
  ; test_row ~row_type:Render_jj_graph.PadRow wrrtuusr "│ │  "
  ; test_row ~row_type:Render_jj_graph.NodeRow vvnqynuv "│ ○  "
  ; test_row ~row_type:Render_jj_graph.LinkRow vvnqynuv "├─╯  "
  ; test_row ~row_type:Render_jj_graph.PadRow noszsqtm "│  "
  ; test_row ~row_type:Render_jj_graph.TermRow noszsqtm "~"
  ; test_row ~row_type:Render_jj_graph.NodeRow noszsqtm "◆  "
  ; test_row ~row_type:Render_jj_graph.PadRow noszsqtm "│  "
  ]
;;

let parsed_rows_with_extra_trailing_vertical_bug () =
  let lpztppmx =
    test_node ~description:"fix elided revisions bug during rebase" "lpztppmx"
  in
  let vxkltmxw = test_node ~description:"(empty) (no description set)" "vxkltmxw" in
  let mwqvkttl = test_node ~description:"(empty) (no description set)" "mwqvkttl" in
  let qqsnmuzr_2 =
    test_node
      ~description:"fix preview mode bugs (I think this broke the update loop somehow)"
      "qqsnmuzr/2"
  in
  let tkozwuzw = test_node ~description:"enable preview mode (5.2 codex)" "tkozwuzw" in
  let nkwwwlnw = test_node ~description:"rewrite" "nkwwwlnw" in
  let voywlxnk = test_node ~description:"(no description set)" "voywlxnk" in
  let wrrtuusr = test_node ~description:"(no description set)" "wrrtuusr" in
  let vvnqynuv = test_node ~description:"(no description set)" "vvnqynuv" in
  let noszsqtm =
    test_node
      ~description:"remove aarch64 linux because it doesn't seem to work"
      "noszsqtm"
  in
  let raw_output =
    {|◆  @@NODE@@
│  @@INFO@@
~  (elided revisions)
│ ○  @@NODE@@
│ │  @@INFO@@
│ ○  @@NODE@@
├─╯  @@INFO@@
│ ○  @@NODE@@
├─╯  @@INFO@@
◆  @@NODE@@
│  @@INFO@@
~  (elided revisions)
│ ○  @@NODE@@
│ │  @@INFO@@
│ ○  @@NODE@@
│ │  @@INFO@@
│ ○  @@NODE@@
│ │  @@INFO@@
│ ○  @@NODE@@
├─╯  @@INFO@@
◆  @@NODE@@
│  @@INFO@@
~|}
  in
  let nodes =
    [
      lpztppmx
    ; vxkltmxw
    ; mwqvkttl
    ; qqsnmuzr_2
    ; tkozwuzw
    ; nkwwwlnw
    ; voywlxnk
    ; wrrtuusr
    ; vvnqynuv
    ; noszsqtm
    ]
  in
  raw_output
  |> Test_native_graph.parse_native_graph_groups
  |> Test_native_graph.attach_nodes_to_native_groups ~nodes
  |> Option.get
;;

let%expect_test "grouping_repro_for_join_row_before_child_node" =
  let groups = rows_with_join_before_child () |> Graph_row_layout.group_rows_by_node in
  List.iteri print_group groups;
  [%expect
    {|
    Group 0 node=top
      head="\226\151\134  "
    Group 1 node=child
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
      cont="\226\148\130  "
    |}]
;;

let%expect_test "rendering_repro_for_join_row_before_child_node" =
  let groups = rows_with_join_before_child () |> Graph_row_layout.group_rows_by_node in
  groups
  |> List.iter (fun group ->
    Graph_row_layout.render_node_group group ~render_content:render_content_lines
    |> List.iter (fun ((row : Render_jj_graph.graph_row_output), img) ->
      Printf.printf "%S => %S\n" row.graph_chars (render_image_to_string img)));
  [%expect
    {|
    "\226\151\134  " => "\226\151\134  top test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  child test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  show conflicts correctly"
    "\226\148\130  " => "\226\148\130"
    |}]
;;

let%expect_test "grouping_repro_for_trailing_vertical_on_terminating_branch" =
  let groups =
    rows_with_elided_termination_case () |> Graph_row_layout.group_rows_by_node
  in
  List.iteri print_group groups;
  [%expect
    {|
    Group 0 node=mmnxzuyv
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 1 node=upnslvuv
      head="\226\151\134  "
      cont="\226\148\130  "
    Group 2 node=upnslvuv/2
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 3 node=lpztppmx
      head="\226\151\134  "
      cont="\226\148\130  "
      cont="~  (elided revisions)"
    Group 4 node=vxkltmxw
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 5 node=mwqvkttl
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 6 node=qqsnmuzr/2
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 7 node=tkozwuzw
      head="\226\151\134  "
      cont="\226\148\130  "
      cont="~  (elided revisions)"
    Group 8 node=nkwwwlnw
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 9 node=voywlxnk
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 10 node=wrrtuusr
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 11 node=vvnqynuv
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 12 node=noszsqtm
      head="\226\151\134  "
      cont="\226\148\130  "
      cont="~"
    |}]
;;

let%expect_test "rendering_repro_for_trailing_vertical_on_terminating_branch" =
  let groups =
    rows_with_elided_termination_case () |> Graph_row_layout.group_rows_by_node
  in
  groups
  |> List.iter (fun group ->
    Graph_row_layout.render_node_group group ~render_content:render_content_lines
    |> List.iter (fun ((row : Render_jj_graph.graph_row_output), img) ->
      Printf.printf "%S => %S\n" row.graph_chars (render_image_to_string img)));
  [%expect
    {|
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  mmnxzuyv test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  show conflicts correctly"
    "\226\151\134  " => "\226\151\134  upnslvuv test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  make bookmarks render origin if need"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  upnslvuv/2 test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  make bookmarks render origin if needed"
    "\226\151\134  " => "\226\151\134  lpztppmx test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  fix elided revisions bug during rebase"
    "~  (elided revisions)" => "~  (elided revisions)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  vxkltmxw test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (empty) (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  mwqvkttl test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  (empty) (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  qqsnmuzr/2 test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  fix preview mode bugs (I think this broke the update loop somehow)"
    "\226\151\134  " => "\226\151\134  tkozwuzw test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  enable preview mode (5.2 codex)"
    "~  (elided revisions)" => "~  (elided revisions)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  nkwwwlnw test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  rewrite"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  voywlxnk test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  wrrtuusr test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  vvnqynuv test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  (no description set)"
    "\226\151\134  " => "\226\151\134  noszsqtm test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  remove aarch64 linux because it doesn't seem to work"
    "~" => "~"
    |}]
;;

let%expect_test "rendering_repro_for_synthetic_terminating_branch_with_extra_vertical" =
  let groups =
    rows_with_synthetic_termination_case () |> Graph_row_layout.group_rows_by_node
  in
  groups
  |> List.iter (fun group ->
    Graph_row_layout.render_node_group group ~render_content:render_content_lines
    |> List.iter (fun ((row : Render_jj_graph.graph_row_output), img) ->
      Printf.printf "%S => %S\n" row.graph_chars (render_image_to_string img)));
  [%expect
    {|
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  mmnxzuyv test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  show conflicts correctly"
    "\226\151\134  " => "\226\151\134  upnslvuv test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  make bookmarks render origin if need"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  upnslvuv/2 test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  make bookmarks render origin if needed"
    "\226\148\130 \226\151\134  " => "\226\148\130 \226\151\134  lpztppmx test@example.com 2024-01-01 deadbeef"
    "~  (elided revisions)" => "~  (elided revisions)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  vxkltmxw test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (empty) (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  mwqvkttl test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  (empty) (no description set)"
    |}]
;;

let%expect_test "rendering_repro_for_extra_trailing_vertical_after_elision" =
  let groups =
    rows_with_extra_trailing_vertical_bug () |> Graph_row_layout.group_rows_by_node
  in
  groups
  |> List.iter (fun group ->
    Graph_row_layout.render_node_group group ~render_content:render_content_lines
    |> List.iter (fun ((row : Render_jj_graph.graph_row_output), img) ->
      Printf.printf "%S => %S\n" row.graph_chars (render_image_to_string img)));
  [%expect
    {|
    "\226\151\134  " => "\226\151\134  lpztppmx test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  fix elided revisions bug during rebase"
    "\226\148\130  " => "\226\148\130"
    "~  (elided revisions)" => "~  (elided revisions)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  vxkltmxw test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  (empty) (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  mwqvkttl test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (empty) (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  qqsnmuzr/2 test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  fix preview mode bugs (I think this broke the update loop somehow)"
    "\226\148\130  " => "\226\148\130"
    "~  (elided revisions)" => "~  (elided revisions)"
    "\226\151\134  " => "\226\151\134  tkozwuzw test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  enable preview mode (5.2 codex)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  nkwwwlnw test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  rewrite"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  voywlxnk test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  wrrtuusr test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  vvnqynuv test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  (no description set)"
    "\226\148\130  " => "\226\148\130"
    "~" => "~"
    "\226\151\134  " => "\226\151\134  noszsqtm test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  remove aarch64 linux because it doesn't seem to work"
    |}]
;;

let%expect_test "parsed_rendering_repro_for_extra_trailing_vertical_after_elision" =
  let groups =
    parsed_rows_with_extra_trailing_vertical_bug () |> Graph_row_layout.group_rows_by_node
  in
  groups
  |> List.iter (fun group ->
    Graph_row_layout.render_node_group group ~render_content:render_content_lines
    |> List.iter (fun ((row : Render_jj_graph.graph_row_output), img) ->
      Printf.printf "%S => %S\n" row.graph_chars (render_image_to_string img)));
  [%expect
    {|
    "\226\151\134  " => "\226\151\134  lpztppmx test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  fix elided revisions bug during rebase"
    "~  (elided revisions)" => "~  (elided revisions)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  vxkltmxw test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (empty) (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  mwqvkttl test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  (empty) (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  qqsnmuzr/2 test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  fix preview mode bugs (I think this broke the update loop somehow)"
    "\226\151\134  " => "\226\151\134  tkozwuzw test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  enable preview mode (5.2 codex)"
    "~  (elided revisions)" => "~  (elided revisions)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  nkwwwlnw test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  rewrite"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  voywlxnk test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  wrrtuusr test@example.com 2024-01-01 deadbeef"
    "\226\148\130 \226\148\130  " => "\226\148\130 \226\148\130  (no description set)"
    "\226\148\130 \226\151\139  " => "\226\148\130 \226\151\139  vvnqynuv test@example.com 2024-01-01 deadbeef"
    "\226\148\156\226\148\128\226\149\175  " => "\226\148\156\226\148\128\226\149\175  (no description set)"
    "\226\151\134  " => "\226\151\134  noszsqtm test@example.com 2024-01-01 deadbeef"
    "\226\148\130  " => "\226\148\130  remove aarch64 linux because it doesn't seem to work"
    "~" => "~"
    |}]
;;

let%expect_test "parsed_grouping_repro_for_extra_trailing_vertical_after_elision" =
  let groups =
    parsed_rows_with_extra_trailing_vertical_bug () |> Graph_row_layout.group_rows_by_node
  in
  List.iteri print_group groups;
  [%expect
    {|
    Group 0 node=lpztppmx
      head="\226\151\134  "
      cont="\226\148\130  "
      cont="~  (elided revisions)"
    Group 1 node=vxkltmxw
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 2 node=mwqvkttl
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 3 node=qqsnmuzr/2
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 4 node=tkozwuzw
      head="\226\151\134  "
      cont="\226\148\130  "
      cont="~  (elided revisions)"
    Group 5 node=nkwwwlnw
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 6 node=voywlxnk
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 7 node=wrrtuusr
      head="\226\148\130 \226\151\139  "
      cont="\226\148\130 \226\148\130  "
    Group 8 node=vvnqynuv
      head="\226\148\130 \226\151\139  "
      cont="\226\148\156\226\148\128\226\149\175  "
    Group 9 node=noszsqtm
      head="\226\151\134  "
      cont="\226\148\130  "
      cont="~"
    |}]
;;
