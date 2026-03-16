(**
   `commit_render_tests.ml`

   Tests for commit rendering with Notty image output.
*)

open Commit_render
open AnsiReverse

(** Render Notty image to string for testing (text-only, no ANSI codes) *)
let image_to_string img =
  let buf = Buffer.create 256 in
  let w, h = Notty.I.(width img, height img) in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (w, h) img;
  Buffer.contents buf
;;

let image_to_ansi_string img =
  Notty.Render.pp_image Format.str_formatter img;
  Format.flush_str_formatter ()
;;

let render_and_print node =
  render_commit_content node |> List.iter (fun img -> print_endline (image_to_string img))
;;

(** Create a test node with specified prefix/rest values *)
let make_test_node
      ?(working_copy = false)
      ?(immutable = false)
      ?(wip = false)
      ?(empty = false)
      ?(is_preview = false)
      ?(hidden = false)
      ?(divergent = false)
      ?(conflict = false)
      ?(bookmarks = [])
      ~change_id_prefix
      ~change_id_rest
      ~commit_id_prefix
      ~commit_id_rest
      ~description
      ()
  =
  Render_jj_graph.
    {
      parents = []
    ; creation_time = Int64.zero
    ; working_copy
    ; immutable
    ; wip
    ; change_id = change_id_prefix ^ change_id_rest
    ; commit_id = commit_id_prefix ^ commit_id_rest
    ; description
    ; bookmarks
    ; author_email = "test@example.com"
    ; author_timestamp = "2024-01-01"
    ; empty
    ; hidden
    ; divergent
    ; conflict
    ; is_preview
    ; change_id_prefix
    ; change_id_rest
    ; commit_id_prefix
    ; commit_id_rest
    }
;;

let%expect_test "render_simple_commit" =
  let node =
    make_test_node
      ~change_id_prefix:"abc"
      ~change_id_rest:"def123"
      ~commit_id_prefix:"123"
      ~commit_id_rest:"456789"
      ~description:"Test commit"
      ()
  in
  render_and_print node;
  [%expect
    {|
    abcdef123 test@example.com 2024-01-01 123456789
    Test commit
    |}]
;;

let%expect_test "render_commit_with_bookmarks" =
  let node =
    make_test_node
      ~change_id_prefix:"abc"
      ~change_id_rest:"def"
      ~commit_id_prefix:"111"
      ~commit_id_rest:"222"
      ~description:"With bookmarks"
      ~bookmarks:[ "main"; "feature" ]
      ()
  in
  render_and_print node;
  [%expect
    {|
    abcdef test@example.com 2024-01-01 main feature 111222
    With bookmarks
    |}]
;;

let%expect_test "render_empty_commit" =
  let node =
    make_test_node
      ~change_id_prefix:"xyz"
      ~change_id_rest:"123"
      ~commit_id_prefix:"aaa"
      ~commit_id_rest:"bbb"
      ~description:"Empty commit"
      ~empty:true
      ()
  in
  let img = render_commit_content node in
  img |> List.iter (fun img -> print_endline (image_to_string img));
  [%expect
    {|
    xyz123 test@example.com 2024-01-01 aaabbb
    (empty) Empty commit
    |}]
;;

let%expect_test "render_working_copy_commit" =
  let node =
    make_test_node
      ~change_id_prefix:"wor"
      ~change_id_rest:"king"
      ~commit_id_prefix:"ccc"
      ~commit_id_rest:"ddd"
      ~description:"Working copy"
      ~working_copy:true
      ()
  in
  render_and_print node;
  [%expect
    {|
    working test@example.com 2024-01-01 cccddd
    Working copy
    |}]
;;

let%expect_test "render_commit_no_rest" =
  let node =
    make_test_node
      ~change_id_prefix:"short"
      ~change_id_rest:""
      ~commit_id_prefix:"min"
      ~commit_id_rest:""
      ~description:"Short IDs"
      ()
  in
  render_and_print node;
  [%expect
    {|
    short test@example.com 2024-01-01 min
    Short IDs
    |}]
;;

let%expect_test "render_multiline_description" =
  let node =
    make_test_node
      ~change_id_prefix:"mul"
      ~change_id_rest:"tiline"
      ~commit_id_prefix:"abc"
      ~commit_id_rest:"def"
      ~description:"First line\nSecond line\nThird line"
      ()
  in
  render_and_print node;
  [%expect
    {|
    multiline test@example.com 2024-01-01 abcdef
    First line
    |}]
;;

let%expect_test "render_no_description" =
  let node =
    make_test_node
      ~change_id_prefix:"nod"
      ~change_id_rest:"esc"
      ~commit_id_prefix:"111"
      ~commit_id_rest:"222"
      ~description:""
      ()
  in
  render_and_print node;
  [%expect
    {|
    nodesc test@example.com 2024-01-01 111222
    (no description set)
    |}]
;;

let%expect_test "render_hidden_duplicate_commit" =
  let node =
    make_test_node
      ~change_id_prefix:"upnslvuv"
      ~change_id_rest:"/2"
      ~commit_id_prefix:"4c63c987"
      ~commit_id_rest:""
      ~description:"make bookmarks render origin if needed"
      ~bookmarks:[ "re-based@origin" ]
      ~hidden:true
      ()
  in
  render_and_print node;
  [%expect
    {|
    upnslvuv/2 test@example.com 2024-01-01 re-based@origin 4c63c987 (hidden)
    make bookmarks render origin if needed
    |}]
;;

let%expect_test "render_divergent_commit" =
  let node =
    make_test_node
      ~change_id_prefix:"lqzzqwqx"
      ~change_id_rest:"/0"
      ~commit_id_prefix:"5ab39974"
      ~commit_id_rest:""
      ~description:"disable worker mode"
      ~bookmarks:[ "main??"; "main@git" ]
      ~divergent:true
      ()
  in
  render_and_print node;
  [%expect
    {|
    lqzzqwqx/0 test@example.com 2024-01-01 main?? main@git 5ab39974 (divergent)
    disable worker mode
    |}]
;;

let%expect_test "render_conflict_and_divergent_commit" =
  let node =
    make_test_node
      ~change_id_prefix:"lqzzqwqx"
      ~change_id_rest:"/0"
      ~commit_id_prefix:"5ab39974"
      ~commit_id_rest:""
      ~description:"disable worker mode"
      ~bookmarks:[ "main??"; "main@git" ]
      ~conflict:true
      ~divergent:true
      ()
  in
  render_and_print node;
  [%expect
    {|
    lqzzqwqx/0 test@example.com 2024-01-01 main?? main@git 5ab39974 (conflict) (divergent)
    disable worker mode
    |}]
;;

let%expect_test "render_divergent_commit_uses_red_styling" =
  let node =
    make_test_node
      ~change_id_prefix:"lqzzqwqx"
      ~change_id_rest:"/0"
      ~commit_id_prefix:"5ab39974"
      ~commit_id_rest:""
      ~description:"disable worker mode"
      ~bookmarks:[ "main??"; "main@git" ]
      ~divergent:true
      ()
  in
  let line1 = render_commit_content node |> List.hd in
  line1
  |> image_to_ansi_string
  |> Parser.parse_ansi_escape_codes
  |> Result.get_ok
  |> List.iter (fun (attr, text) ->
    if String.trim text <> ""
    then (
      AnsiReverse.Internal.print_attr attr;
      Printf.printf "Text: %S\n" text));
  [%expect
    {|
    attr:
    \e[0m<\e[0;31;1mATTR\e[0m\e[K\e[0m>\e[0m
    Text: "lqzzqwqx"
    attr:
    \e[0m<\e[0;31mATTR\e[0m\e[K\e[0m>\e[0m
    Text: "/0"
    attr:
    \e[0m<\e[0;33mATTR\e[0m\e[K\e[0m>\e[0m
    Text: " test@example.com"
    attr:
    \e[0m<\e[0;36mATTR\e[0m\e[K\e[0m>\e[0m
    Text: " 2024-01-01"
    attr:
    \e[0m<\e[0;35mATTR\e[0m\e[K\e[0m>\e[0m
    Text: " main?? main@git"
    attr:
    \e[0m<\e[0;34;1mATTR\e[0m\e[K\e[0m>\e[0m
    Text: " 5ab39974"
    attr:
    \e[0m<\e[0;31mATTR\e[0m\e[K\e[0m>\e[0m
    Text: " (divergent)"
    |}]
;;

let%expect_test "graph_node_attr_divergent_is_red_bold" =
  let node =
    make_test_node
      ~change_id_prefix:"lqzzqwqx"
      ~change_id_rest:"/0"
      ~commit_id_prefix:"5ab39974"
      ~commit_id_rest:""
      ~description:"disable worker mode"
      ~divergent:true
      ()
  in
  node |> graph_node_attr |> AnsiReverse.Internal.print_attr;
  [%expect
    {|
    attr:
    \e[0m<\e[0;31;1mATTR\e[0m\e[K\e[0m>\e[0m
    |}]
;;
