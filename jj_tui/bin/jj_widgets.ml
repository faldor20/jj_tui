open Notty
open Nottui
open Lwd_infix
open Jj_tui
open! Util
module W = Nottui_widgets
module Wd = Widgets

module Make (Vars : Global_vars.Vars) = struct
  open Jj_process.Make (Vars)

  exception Found

  let elieded_symbol =
    let a = String.get_utf_8_uchar "◌" 0 in
    if a |> Uchar.utf_decode_is_valid
    then a |> Uchar.utf_decode_uchar
    else failwith "not a unicode string"
  ;;

  let test_data =
    {|◉    yzquvpvl eli.jambu@gmail.com 2024-05-23 15:04:24 3565237c
├─╮  merger
◉ │  wttqrrwo eli.jambu@gmail.com 2024-05-23 14:36:43 ui-update* 7e46fdef
│ │  border_box working
│ │ ◉  skwqmzmt eli.jambu@gmail.com 2024-05-25 01:07:57 7e358c79
│ ├─╯  test old size scaling
│ ◉  nuptyuws eli.jambu@gmail.com 2024-05-22 18:58:31 master 7b156964
│ │  Update README.md
│ ◌  (elided revisions)
│ │ ◉  kmslutyl eli.jambu@gmail.com 2024-05-22 18:07:36 7b10ea4f conflict
│ │ │  (no description set)
│ │ ◉  nqyzyups eli.jambu@gmail.com 2024-05-22 18:07:36 519c664f conflict
│ ├─╯  progress
│ ◉  tutrxvzs eli.jambu@gmail.com 2024-05-22 18:07:36 af8620df
│ │  flakes working
│ ◌  (elided revisions)
│ │ ◉  vlkxvssz eli.jambu@gmail.com 2024-05-15 19:40:28 79fb16f1
│ ├─╯  (no description set)
│ ◉  vtkpsqlr eli.jambu@gmail.com 2024-05-15 16:44:09 1974cc7d
│ │  switch to pkgsStatic
│ ◌  (elided revisions)
│ │ ◉  zpvnoqkm eli.jambu@gmail.com 2024-05-15 09:47:41 c9f95816
├───╯  (no description set)
◉ │  unspmqrw eli.jambu@gmail.com 2024-05-15 09:47:41 83aafe3c
├─╯  update to nottui
│ ◉  smvuxtrv eli.jambu@gmail.com 2024-05-15 09:47:41 d0ce4665
├─╯  (empty) bup
◉  yqytskyk eli.jambu@gmail.com 2024-05-15 09:47:41 0a89ce77
│  test reorganise
◌  (elided revisions)
│ ◉  qpqzkuss eli.jambu@gmail.com 2024-05-15 09:46:18 fdd16b26 conflict
│ │  (no description set)
│ ◉  xpqmtrmp eli.jambu@gmail.com 2024-05-15 09:46:18 555a5355
├─╯  remove old nix file
◉  zxpskuop eli.jambu@gmail.com 2024-05-15 09:46:18 dac0f8bb
│  Update README.md|}
  ;;

  let rec list_to_pairs lst =
    match lst with
    | [] | [ _ ] ->
      [] (* If list is empty or has only one element, return empty list *)
    | x :: y :: rest ->
      (x, y) :: list_to_pairs rest
  ;;

  let rec pairwise f ~f_last lst =
    match lst with
    | [ single ] ->
      f_last single
    | x :: y :: rest ->
      ((x, y) |> f) :: pairwise f ~f_last rest
    | [] ->
      [] (* If list is empty or has only one element, return empty list *)
  ;;

  let seperate_revs () =
    let graph =
      jj_no_log [ "log" ]
      |> String.split_on_char '\n'
      (* filter out any lines that contain *)
      |> List.filter (fun x ->
        if x |> String.length <= 1
        then false
        else (
          try
            x
            |> String.iteri (fun i char ->
              let char = String.get_utf_8_uchar x i |> Uchar.utf_decode_uchar in
              if char |> Uchar.equal elieded_symbol then raise Found else ());
            true
          with
          | _ ->
            false))
      |> pairwise (fun (top, descr) -> top ^ "\n" ^ descr) ~f_last:(fun last -> [ last ])
    in
    let revs =
      jj_no_log ~color:false [ "log"; "--no-graph"; "-T"; {|change_id++"\n"|} ]
      |> String.split_on_char '\n'
      |> List.filter (fun x -> x |> String.trim <> "")
    in
    if List.length graph <> List.length revs
    then
      failwith
      @@ "When getting list of revs the graph had a different number of items to the \
          revs. This shouldn't be possible and is a bug. "
      ^ Printf.sprintf "revs:%d graph:%d" (List.length revs) (List.length graph)
      ^ "\n"
      ^ String.concat "\n" revs
      ^ String.concat "\n" graph;
    graph, revs
  ;;

  let selection_list ?(focus = Focus.make ()) items =
    Focus.request focus;
    let selected_var = Lwd.var 0 in
    let items =
      let$ selected = Lwd.get selected_var
      and$ focus = focus |> Focus.status
      and$ items = items in
      items
      |> List.mapi (fun i x -> if selected == i then x true else x false)
      |> Ui.vcat
      |> Ui.keyboard_area ~focus (function
        | `Arrow `Up, [] ->
          selected_var $= max (Lwd.peek selected_var - 1) 0;
          `Handled
        | `Arrow `Down, [] ->
          selected_var $= min (Lwd.peek selected_var + 1) ((items |> List.length) - 1);
          `Handled
        | _ ->
          `Unhandled)
    in
    items
  ;;

  (**A weird function to replace the start of a string if it matches the prefix. Only replacing at most as much as the matched prefix *)
  let unsafe_blit_start_if prefix replace string =
    if String.starts_with string ~prefix
    then (
      (*TODO could do this unsafe for more speed*)
      let padding = (replace |> String.length) - (prefix |> String.length) in
      let string = String.make padding ' ' ^ string in
      Bytes.blit_string
        replace
        0
        (*we pad it to make sure we aren't going to blit over more than the prefix*)
        (Bytes.unsafe_of_string string)
        0
        (replace |> String.length);
      string)
    else string
  ;;

  (*TODO:make a custom widget the renders the commit with and without selection.
    with selection replace the dot with a blue version and slightly blue tint the background *)
  let revs_list () =
    seperate_revs ()
    |> fst
    |> List.map (fun x is_focused ->
      (*hightlight blue when selection is true*)
      let prefix =
        if is_focused then I.char A.(bg A.blue) '>' 1 2 else I.char A.empty ' ' 1 2
      in
      I.hcat
        [
          prefix;
          x ^ "\n" |> unsafe_blit_start_if "@" "◉" |> Jj_tui.AnsiReverse.colored_string;
        ]
      |> Ui.atom)
    |> Lwd.pure
    |> selection_list
  ;;

  (*TODO:make a custom widget the renders the commit with and without selection.
    with selection replace the dot with a blue version and slightly blue tint the background *)
  let revs_list_filtered () =
    let graph_items, revs = seperate_revs () in
    graph_items
    |> List.map (fun x is_focused ->
      (* hightlight blue when selection is true *)
      let prefix =
        if is_focused then I.char A.(bg A.blue) '>' 1 2 else I.char A.empty ' ' 1 2
      in
      I.hcat
        [
          prefix;
          x ^ "\n" |> unsafe_blit_start_if "@" "◉" |> Jj_tui.AnsiReverse.colored_string;
        ]
      |> Ui.atom)
    |> List.map2 (fun rev ui -> Wd.{ ui; data = rev }) revs
    |> Wd.filterable_selection_list ~on_confirm:(fun _->()) ~filter_predicate:(fun input rev ->
      rev |> String.starts_with ~prefix:input)
  ;;
end
