(**
   `commit_render_tests.ml`

   Tests for commit rendering with Notty image output.
*)

open Commit_render

(** Render Notty image to string for testing (text-only, no ANSI codes) *)
let image_to_string img =
  let buf = Buffer.create 256 in
  let w, h = Notty.I.(width img, height img) in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (w, h) img;
  Buffer.contents buf
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
    ; hidden = false
    ; divergent = false
    ; conflict = false
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
