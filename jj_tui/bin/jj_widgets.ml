open Notty
open Nottui
open Lwd_infix
open Jj_tui
open! Util
module W = Nottui_widgets

(** Collection of JJ specific widgets*)
module Wd = Widgets

module Make (Vars : Global_vars.Vars) = struct
  open Vars
  open Global_vars
  open Jj_process.Make (Vars)

  exception FoundStart
  exception FoundFiller

  let make_uchar str =
    let a = String.get_utf_8_uchar str 0 in
    if a |> Uchar.utf_decode_is_valid
    then a |> Uchar.utf_decode_uchar
    else failwith "not a unicode string"
  ;;

  let elieded_symbol = make_uchar "◌"
  let rev_symbol = make_uchar "◉"

  let is_whitespace_char (code_point : int) : bool =
    match code_point with
    | 0x0009 (* Tab *)
    | 0x000A (* Line Feed *)
    | 0x000B (* Vertical Tab *)
    | 0x000C (* Form Feed *)
    | 0x000D (* Carriage Return *)
    | 0x0020 (* Space *)
    | 0x0085 (* Next Line *)
    | 0x00A0 (* No-Break Space *)
    | 0x1680 (* Ogham Space Mark *)
    | 0x2000 (* En Quad *)
    | 0x2001 (* Em Quad *)
    | 0x2002 (* En Space *)
    | 0x2003 (* Em Space *)
    | 0x2004 (* Three-Per-Em Space *)
    | 0x2005 (* Four-Per-Em Space *)
    | 0x2006 (* Six-Per-Em Space *)
    | 0x2007 (* Figure Space *)
    | 0x2008 (* Punctuation Space *)
    | 0x2009 (* Thin Space *)
    | 0x200A (* Hair Space *)
    | 0x2028 (* Line Separator *)
    | 0x2029 (* Paragraph Separator *)
    | 0x202F (* Narrow No-Break Space *)
    | 0x205F (* Medium Mathematical Space *)
    | 0x3000 (* Ideographic Space *) ->
      true
    | _ ->
      false
  ;;

  let is_graph_start_char char =
    let i = Uchar.to_int char in
    (*chars like these: ├─╮*)
    let is_pipe = i > 0x2500 && i < 0x259f in
    let is_whitespace = is_whitespace_char i in
    is_pipe || is_whitespace
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

  (*
     1. Make the graph
  *)
  let is_line_filler line =
    line
    (* We will iterate through skipping any chars like pipes and whitespace untill we find either:
       a) A rev start char,which would make the line a rev.
       b) Nothing, which would make the the line filler
    *)
    |> String.iteri (fun i char ->
      let uchar = String.get_utf_8_uchar line i |> Uchar.utf_decode_uchar in
      (*I've removed the part that tries to precisely skip all the start chars. this is becasue it gets all stuffed up by the terminal escape codes
        FIXME currently this will get stuffed up if a line has that rev symbol in it
      *)

      (* if not (uchar |> is_graph_start_char) *)
      (* then *)
      if uchar |> Uchar.equal rev_symbol || char == '@' then raise FoundStart
      (* else raise FoundFiller *)
      (* else () *))
  ;;

(** Function to tag duplicated items in a list *)
let tag_duplicates lst =
  (* Create a frequency map to count occurrences of each element *)
  let freq_map =
    List.fold_left (fun acc {change_id;_} ->
      let count = try List.assoc change_id acc with Not_found -> 0 in
      (change_id, count + 1) :: List.remove_assoc change_id acc
    ) [] lst
  in
  (* Tag each item in the list based on the frequency map *)
  List.map (fun ({change_id;_} as x) ->
    if List.assoc change_id freq_map > 1 then Duplicate x else Unique x 
  ) lst

  (**Returns a list of revs with both the change_id and commit_id*)
  let get_revs () =
    jj_no_log
      ~color:false
      [ "log";  "-T"; {|"|"++change_id++"|"++commit_id++"\n"|} ]
    |> String.split_on_char '\n'
    |> List.filter_map (fun x ->
      let items = x |> String.split_on_char '|' in
      match items with
      | [ _graph; change_id; commit_id ] ->
        Some { change_id; commit_id }
      | _ ->
        None)
    |>tag_duplicates
    |> Array.of_list
  ;;

  (** returns the graph and a list of revs within that graph*)
  let graph_and_revs () =
    let graph =
      jj_no_log [ "log" ]
      |> String.split_on_char '\n'
      (* filter out any lines that contain *)
      |> Base.List.fold ~init:([], None) ~f:(fun (new_list, last) x ->
        if x |> String.length <= 1
        then `Filler x :: new_list, None
        else (
          match last with
          | Some last_line ->
            `Selectable (String.concat "\n" [ last_line; x ]) :: new_list, None
          | None ->
            (try
               is_line_filler x;
               `Filler x :: new_list, None
             with
             | FoundStart ->
               new_list, Some x
             | FoundFiller ->
               `Filler x :: new_list, None)))
      |> fst
      |> List.rev
      |> Array.of_list
    in
    let revs = get_revs () in
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

  (* (*TODO:make a custom widget the renders the commit with and without selection. *)
     (* with selection replace the dot with a blue version and slightly blue tint the background *) *)
  (* let revs_list_filtered () = *)
  (* let graph_items, revs = seperate_revs () in *)
  (* graph_items *)
  (* |> List.map (fun x is_focused -> *)
  (* hightlight blue when selection is true *)
  (* let prefix = *)
  (* if is_focused then I.char A.(bg A.blue) '>' 1 2 else I.char A.empty ' ' 1 2 *)
  (* in *)
  (* I.hcat *)
  (* [ *)
  (* prefix *)
  (* ; x ^ "\n" |> unsafe_blit_start_if "@" "◉" |> Jj_tui.AnsiReverse.colored_string *)
  (* ] *)
  (* |> Ui.atom) *)
  (* |> List.map2 (fun rev ui -> Wd.{ ui; data = rev }) revs *)
  (* |> Lwd.pure *)
  (* |> Wd.filterable_selection_list *)
  (* ~on_confirm:(fun _ -> ()) *)
  (* ~filter_predicate:(fun input rev -> rev |> String.starts_with ~prefix:input) *)
  (* ;; *)

  (** Start a process that will take full control of both stdin and stdout.
      This is used for interactive diffs and such*)
  let interactive_process env cmd =
    let post_change new_view =
      Global_funcs.update_status ();
      Lwd.set ui_state.view new_view
    in
    let exit_status_to_str y =
      match match y with `Exited x -> x | `Signaled x -> x with
      | 0 ->
        "success"
      | 1 ->
        "failure%s"
      | a ->
        Printf.sprintf "unknown code %d" a
    in
    let res = switch_to_process env cmd in
    let$ ui =
      W.vbox
        [
          W.string (Printf.sprintf "exit code:%s" (res |> exit_status_to_str)) |> Lwd.pure
        ; W.button "back to main UI" (fun _ -> post_change `Main) |> Lwd.pure
        ]
    in
    ui
    |> Ui.keyboard_area (fun event ->
      match event with
      | `ASCII ' ', _ ->
        post_change `Main;
        `Handled
      | _ ->
        `Unhandled)
  ;;
end
