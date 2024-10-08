(** Collection of JJ specific widgets*)

open Notty
open Nottui
open Lwd_infix
open! Util
open Process
open Logging
open Picos_std_structured

exception FoundStart
exception FoundFiller

let make_uchar str =
  let a = String.get_utf_8_uchar str 0 in
  if a |> Uchar.utf_decode_is_valid
  then a |> Uchar.utf_decode_uchar
  else failwith "not a unicode string"
;;

let elieded_symbol = make_uchar "◌"
let elieded_symbol_alt = make_uchar "○"
let rev_symbol = make_uchar "◉"
let merge_symbol = make_uchar "◆"

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
  | 0x202F (* Narrow No-Brpeak Space *)
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
  if line |> Base.String.is_substring ~substring:"root()"
  then raise FoundStart
  else
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
      (* TODO: Overhaul the default graph template to include a special token at the end of selectable revisions *)
      if (uchar |> Uchar.equal merge_symbol
          || uchar |> Uchar.equal rev_symbol
          || uchar |> Uchar.equal elieded_symbol
          || uchar |> Uchar.equal elieded_symbol_alt
          || uchar |> Uchar.equal (make_uchar "×")
          || char == '@')
         && not (line |> Base.String.is_substring ~substring:"(elided revisions)")
      then raise FoundStart)
;;

(** Function to tag duplicated items in a list *)
let tag_duplicates lst =
  (* Create a frequency map to count occurrences of each element *)
  let freq_map =
    List.fold_left
      (fun acc { change_id; _ } ->
        let count = try List.assoc change_id acc with Not_found -> 0 in
        (change_id, count + 1) :: List.remove_assoc change_id acc)
      []
      lst
  in
  (* Tag each item in the list based on the frequency map *)
  List.map
    (fun ({ change_id; _ } as x) ->
      if List.assoc change_id freq_map > 1 then Duplicate x else Unique x)
    lst
;;

let find_selectable_from_graph str =
  let selectable_count = ref 0 in
  let processLine new_list previous_line this_line =
    match previous_line with
    | Some previous_line ->
      selectable_count := !selectable_count + 1;
      `Selectable (String.concat "\n" [ previous_line; this_line ]) :: new_list, None
    | None ->
      (try
         is_line_filler this_line;
         `Filler this_line :: new_list, None
       with
       | FoundStart ->
         new_list, Some this_line
       | FoundFiller ->
         `Filler this_line :: new_list, None)
  in
  let graph =
    str
    |> String.split_on_char '\n'
    (* filter out any lines that contain *)
    |> Base.List.fold ~init:([], None) ~f:(fun (new_list, previous) x ->
      (*there is generally a final newline and we should just skip that *)
      if String.length x = 0
      then new_list, previous
      else if String.length x <= 1
      then `Filler x :: new_list, None
      else processLine new_list previous x)
    (*the root() commit only has one line and will always be last, so we will try to process the final line*)
    |> (fun (list, final_line) ->
         match final_line with
         | Some line ->
           selectable_count := !selectable_count + 1;
           `Selectable line :: list
         | None ->
           list)
    |> List.rev
    |> Array.of_list
  in
  !selectable_count, graph
;;

(** retrieve revs from jj log of jj_tui*)
let revs_from_log log =
  log
  |> String.split_on_char '\n'
  |> List.filter_map (fun x ->
    let items = x |> String.split_on_char '|' in
    match items with
    | [ _graph; change_id; commit_id ] ->
      Some { change_id; commit_id }
    | _ ->
      None)
  |> tag_duplicates
  |> Array.of_list
;;

module Make (Process : sig
    val jj_no_log :
       ?get_stderr:bool
      -> ?snapshot:bool
      -> ?color:bool
      -> string list
      -> string
  end) =
struct
  open Process

  (**Returns a list of revs with both the change_id and commit_id*)
  let get_revs ?revset () =
    let revset_arg = match revset with Some revset -> [ "-r"; revset ] | None -> [] in
    jj_no_log
      ~color:false
      ([ "log"; "-T"; {|"|"++change_id++"|"++commit_id++"\n"|} ] @ revset_arg)
    |> revs_from_log
  ;;

  (* let test= *)
  (* let count,graph=find_selectable_from_graph root_test *)
  (* in *)
  (* if count<=0 then failwith "no process root" *)
  (* ;; *)


  (** returns the graph and a list of revs within that graph*)
  let graph_and_revs ?revset () =
    (*We join_after here to ensure any errors in sub-fibers only propegate to here, otherwise fibers everywhere would get cancelled when an error here occurs*)
    Flock.join_after @@ fun _->
    let graph =
      Flock.fork_as_promise @@ fun () ->
      let revset_arg = match revset with Some revset -> [ "-r"; revset ] | None -> [] in
      let output = jj_no_log ([ "log" ] @ revset_arg) in
      output |> find_selectable_from_graph
    and revs = Flock.fork_as_promise @@ fun () -> get_revs ?revset () in
    let selectable_count, graph = Promise.await graph
    and revs = Promise.await revs in
    (*The graph should never have selectable items that don't also have a rev*)

    (* TODO: remove this becasue it's just for debugging*)
    let revs_len = revs |> Array.length in
    if selectable_count <> revs_len
    then failwith (Printf.sprintf "selectable:%d revs:%d" selectable_count revs_len);
    graph, revs
  ;;
end

(*========Tests======*)

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

let root_test = {|◆  zzzzzzzz root() 00000000|}

let test_data_2 =
  {|@  qpnrvwyl ethanboxx 13 seconds ago 48829167
│  (no description set)
◆  pqvkrmkw ethanboxx 3 months ago main v0.1.10 HEAD@git 931019c4
│  (empty) Merge pull request #63 from eopb/release-plz-2024-05-11T09-14-47Z
~  (elided revisions)
│ ○  xnyvmlur ethanboxx 5 months ago push-znvwmrtqnnlq 0deaa0aa
├─┘  feat: impl `Deref` and `DerefMut` without exposing `Secret`
◆  kkzuqwxo ethanboxx 5 months ago 9aa340cc
│  (empty) Merge pull request #56 from eopb/push-smksztlxprww
~|}
;;

let%expect_test "revs_graph_parsing" =
  let selectable, graph = find_selectable_from_graph test_data_2 in
  graph
  |> Array.iter (fun x ->
    match x with
    | `Filler x ->
      "F:" |> print_endline;
      x |> print_endline
    | `Selectable x ->
      "S:" |> print_endline;
      x |> print_endline);
  [%expect
    {|
    S:
    @  qpnrvwyl ethanboxx 13 seconds ago 48829167
    │  (no description set)
    S:
    ◆  pqvkrmkw ethanboxx 3 months ago main v0.1.10 HEAD@git 931019c4
    │  (empty) Merge pull request #63 from eopb/release-plz-2024-05-11T09-14-47Z
    F:
    ~  (elided revisions)
    S:
    │ ○  xnyvmlur ethanboxx 5 months ago push-znvwmrtqnnlq 0deaa0aa
    ├─┘  feat: impl `Deref` and `DerefMut` without exposing `Secret`
    S:
    ◆  kkzuqwxo ethanboxx 5 months ago 9aa340cc
    │  (empty) Merge pull request #56 from eopb/push-smksztlxprww
    F:
    ~ |}]
;;
