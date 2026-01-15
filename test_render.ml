open Jj_tui
open Notty
open Notty.I

let test_node : Render_jj_graph.node =
  {
    parents = [];
    creation_time = Int64.zero;
    working_copy = true;
    immutable = false;
    wip = false;
    change_id = "ztooztwk";
    commit_id = "235795c5";
    description = "(no description set)";
    bookmarks = [];
    author_email = "eli.jambu@gmail.com";
    author_timestamp = "2026-01-15 14:05:59";
    empty = true;
    hidden = false;
    divergent = false;
    is_preview = false;
  }

let render_commit (node : Render_jj_graph.node) : image =
  let open Notty.A in
  let styled_text attr text = string attr text in
  let change_id_short =
    String.sub node.change_id 0 (min 8 (String.length node.change_id))
  in
  let commit_id_short =
    String.sub node.commit_id 0 (min 8 (String.length node.commit_id))
  in
  let description_line =
    match String.split_on_char '\n' node.description with
    | first :: _ when String.trim first <> "" -> String.trim first
    | _ -> "(no description set)"
  in

  let line1 =
    hcat
      [
        styled_text (fg lightcyan ++ st bold) change_id_short;
        styled_text (fg white ++ st dim) (" " ^ node.author_email);
        styled_text (fg white ++ st dim) (" " ^ node.author_timestamp);
        styled_text (fg cyan ++ st dim) (" " ^ commit_id_short);
      ]
  in

  let description_with_prefix =
    if node.empty then "(empty) " ^ description_line else description_line
  in
  let line2 = styled_text (fg white ++ st dim) description_with_prefix in
  vcat [ line1; line2 ]

let () =
  let content = render_commit test_node in
  let graph = string A.empty "@  " in

  Printf.printf "Content height: %d\n" (height content);
  Printf.printf "Content width: %d\n" (width content);
  Printf.printf "\nExpected output:\n";
  Printf.printf "@  ztooztwk eli.jambu@gmail.com 2026-01-15 14:05:59 235795c5\n";
  Printf.printf "│  (empty) (no description set)\n";
  Printf.printf "\nActual rendering test:\n";

  let node_glyphs = [ "○"; "@"; "◌"; "◆" ] in
  let graph_continuation =
    let chars = "@  " in
    let replaced = ref chars in
    List.iter
      (fun glyph ->
        replaced := Str.global_replace (Str.regexp_string glyph) "│" !replaced)
      node_glyphs;
    string A.empty !replaced
  in

  let lines =
    List.init (height content) (fun i ->
        let line_img = vcrop i 1 content in
        if i = 0 then hcat [ graph; line_img ]
        else hcat [ graph_continuation; line_img ])
  in
  let result = vcat lines in

  Printf.printf "Result height: %d\n" (height result);
  Printf.printf "Result width: %d\n" (width result);

  ()
