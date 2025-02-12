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

(*
   let ansi_regex =
   let open Re in
   let csi_start = alt [ char '\x1B'; char '\x9B' ] in  (* ESC or CSI *)
   let intermediates = rep (set "[]()#;?") in
   let params = rep (seq [ char ';'; rep1 (set "-a-zA-Z0-9/#&.:=?%@~_") ]) in
   let csi_terminator = alt [
    char '\x07';  (* BEL *)
    seq [ char '\x1B'; char '\\' ];  (* ESC \ *)
    char '\x9C'  (* ST *)
  ] in
   let csi_sequence = seq [
    csi_start;
    intermediates;
    opt (alt [
      seq [ params; csi_terminator ];
      seq [ rep1 (rg 'a' 'z'); opt params; csi_terminator ]
    ])
  ] in
   let param_sequence = seq [
    rep (seq [ rg '0' '9'; rep (seq [ char ';'; rep (rg '0' '9') ]) ]);
    alt [
      set "A-PR-TZ";  (* Cursor movements and scroll regions *)
      set "cf-nq-uy";  (* Tab stops and erasure *)
      set "=><~"       (* Keypad and editing modes *)
    ]
  ] in
   Re.compile (alt [
    csi_sequence;
    param_sequence
  ])
   let ansi_regex =
   let open Re in
   let st = alt [ str "\x07"; str "\x1B\\"; str "\x9C" ] in
   let pattern =
   alt [
        (* CSI escape sequence pattern *)
        seq [
          set "\x1B\x9B";
          rep (set "[]()#;?");
          opt (alt [
            rep (seq [ char ';'; rep1 (set "a-zA-Z0-9/#&.:=?%@~_-") ]);
            seq [
              rep1 (set "a-zA-Z0-9");
              rep (seq [ char ';'; rep (set "a-zA-Z0-9/#&.:=?%@~_-") ])
            ]
          ]);
          st;
        ];
        
        (* OSC escape sequence pattern *)
        seq [
          opt (seq [ repn digit 1 (Some 4); rep (seq [ char ';'; repn digit 0 (Some 4) ]) ]);
          set "0-9A-PR-TZcf-nq-uy=><~";
        ]
      ]
   in
   Re.compile pattern *)
let ansi_regex =
  let open Re in
  let pattern =
    seq
      [
        set "\x1B"
      ; (* ESC character *)
        char '['
      ; (* Opening bracket *)
        rep
          ((* Zero or more of: *)
           alt
             [ digit; (* Digits *) char ';' (* Semicolon separator *) ])
      ; char 'm' (* SGR terminator *)
      ]
  in
  Re.compile pattern
;;

let remove_ansi str = str |> Re.replace_string ~by:"" ansi_regex
let count_ansi str = str |> Re.all ansi_regex |> List.length

let find_selectable_from_graph str =
  let matches =
    str
    |> Re.split_full
         (Re.Pcre.regexp
            ~flags:[ Re.Pcre.(`MULTILINE) ]
            {|(^.*?)\$\$--START--\$\$\|(.+?)\|(.+?)\|([\s\S]*?)\$\$--END--\$\$\n?|})
  in
  (*if there are no matches it's all filler*)
  (* let count = matches |> List.length in *)
  let graph, ids =
    matches
    |> List.fold_left
         (fun (graph_acc, ids_acc) chunk ->
           match chunk with
           | `Delim selectable ->
             let graph_bit = Re.Group.get selectable 1 in
             let change_id = Re.Group.get selectable 2 |> remove_ansi in
             let commit_id = Re.Group.get selectable 3 |> remove_ansi in
             let content = Re.Group.get selectable 4 in
             ( `Selectable (graph_bit ^ content) :: graph_acc
             , { change_id; commit_id } :: ids_acc )
           | `Text filler ->
             if filler = ""
             then
               graph_acc, ids_acc
             else `Filler filler :: graph_acc, ids_acc)
         ([], [])
    |> fun (graph, ids) -> List.rev graph |> Array.of_list, List.rev ids
  in
  let revs = ids |> tag_duplicates in
  graph, revs
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

  let base_graph_template =
    {|if(root,
  format_root_commit(self),
  label(if(current_working_copy, "working_copy"),
    concat(
      format_short_commit_header(self)++"\n" ,
      separate(" ",
        if(empty, label("empty", "(empty)")),
        if(description,
          description.first_line(),
          label(if(empty, "empty"), description_placeholder),
        ),
      ) ,
    ),
  )
)|}
  ;;

  let graph_info_template =
    {|"$$--START--$$"++"|"++change_id++"|"++commit_id++"|"++|}
    ^ base_graph_template
    ^ {|++"$$--END--$$"++""|}
  ;;

  let get_graph_info revset_arg =
    let output = jj_no_log ([ "log"; "-T"; graph_info_template ] @ revset_arg) in
    output |> find_selectable_from_graph
  ;;

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
  let graph_view_template = {|""|}

  (** returns the graph and a list of revs within that graph*)
  let graph_and_revs ?revset () =
    (*We join_after here to ensure any errors in sub-fibers only propegate to here, otherwise fibers everywhere would get cancelled when an error here occurs*)
    Flock.join_after @@ fun _ ->
    let graph =
      Flock.fork_as_promise @@ fun () ->
      let revset_arg = match revset with Some revset -> [ "-r"; revset ] | None -> [] in
      get_graph_info revset_arg
    in
    let graph, revs = Promise.await graph in
    (*The graph should never have selectable items that don't also have a rev*)
    (* if not (Array.length graph = List.length revs) then
      failwith @@ Printf.sprintf "graph and revs length mismatch %d %d" (Array.length graph) (List.length revs)
    else *)
    graph, revs |> Array.of_list
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

let test_data_3 =
  {|@  $$--START--$$|zqtxnkuuryqzzolyksrylpzotmplmvus|8ee443e4a374f7dfdd00494d8bf71af6162a1300|zqtxnkuu eli.jambu@gmail.com 2025-02-15 21:22:48 8ee443e4
│  (no description set)$$--END--$$
◌  $$--START--$$|wmrnukwqvmnrsovsyxnmpwlkklnzpvpp|b72673dcf464650064cdd80233b47848503cd01a|wmrnukwq eli.jambu@gmail.com 2025-02-12 20:20:28 git_head() b72673dc
│  wip: test$$--END--$$
○  $$--START--$$|oxpkqyozkvtxoklqutxztsmznxvwroxx|9c3af61798927da4c80caa216688ea9d69e0d8bb|oxpkqyoz eli.jambu@gmail.com 2024-11-26 12:13:41 9c3af617
│  added duplicate and undo commands$$--END--$$
○  $$--START--$$|tmyqqryzkukzztrpxrsxlqlzutptsnsw|47d6c9a1db9d3756549dc7a68690f3df5600c83a|tmyqqryz eli.jambu@gmail.com 2024-11-23 21:50:19 47d6c9a1
│  make update view fully async$$--END--$$
│ ○  $$--START--$$|xwxrzsrzzlttpmysywysmnvtmrvysrvy|8e338e5c76cdcfb71951367ff87eb1df98561fbd|xwxrzsrz eli.jambu@gmail.com 2024-11-23 21:17:50 8e338e5c
│ │  use picos nottui$$--END--$$
│ ○  $$--START--$$|osuupotwtypnmvnkrrlxmyxukvykmtsn|3c203669a359b27833d9104c80505afd89af5f4d|osuupotw eli.jambu@gmail.com 2024-11-23 19:25:59 3c203669
│ │  support await_read for async integration with nottui$$--END--$$
│ ◌  $$--START--$$|zmxzlvmwmnqruvrosmmknumpupoztuum|668152f47d627e80184b8dbb5f40dd2b83ad6488|zmxzlvmw eli.jambu@gmail.com 2024-11-11 12:06:45 668152f4
├─╯  wip: try to allow nottui to support picos$$--END--$$
│ ○  $$--START--$$|opytqrnrrxlkzsxtkuvtvxyrpqsmrznl|e9d81817d5e56e0817b725e2965dd36d3918a087|opytqrnr eli.jambu@gmail.com 2024-11-02 12:31:37 e9d81817
├─╯  safeguard obj.magic functions$$--END--$$
│ ○  $$--START--$$|ozotxprmvvwvwuulxlokrmlksxtzpmmv|e882ff4a65c0eea778c4d0cc572d63e85444bc93|ozotxprm eli.jambu@gmail.com 2024-11-01 21:03:37 test-issues* e882ff4a
├─╯  spooky testing branch indicator$$--END--$$
◆  $$--START--$$|kyzmstkmrsnrvtzzpwwnummnzpwlousx|15e7195328f95c7158dd1adc5e4bb50c7dd3372a|kyzmstkm eli.jambu@gmail.com 2024-11-01 21:03:37 master v0.8.8 v0.8.9 15e71953
│  fix remaining references to branch rather than bookmark$$--END--$$
~  (elided revisions)
│ ○  $$--START--$$|uzuylryqmsmrlyzunluznwlkqsuurktp|811f78b9f5d3272ff65a80d973ad9fe3db338fc8|uzuylryq eli.jambu@gmail.com 2024-10-31 18:49:04 811f78b9
├─╯  Try to make new dune build the project$$--END--$$
◆  $$--START--$$|oorzkzkwlkqpptmnzvvqvkmxxxrvxpnv|93c69eccd3e0838ee45946dc2b0eadbe4e679362|oorzkzkw eli.jambu@gmail.com 2024-10-27 18:44:00 aaa v0.8.7 93c69ecc
│  updated to use bookmark instead of branch$$--END--$$
~  (elided revisions)
│ ○  $$--START--$$|nyzlmxtpvrxsormrrtzkwsopzuypppwr|81fca5ab7626736be4f61323ca8f27ed35659343|nyzlmxtp eli.jambu@gmail.com 2024-09-30 21:13:28 81fca5ab
├─╯  debugging config$$--END--$$
◆  $$--START--$$|llxznmqxrmtumwyprntkvzupkywpvwoz|a529037b79b469d3c63857ef70395be46a38dda7|llxznmqx eli.jambu@gmail.com 2024-09-30 20:57:49 a529037b
│  multi-select$$--END--$$
~  (elided revisions)
│ ○  $$--START--$$|wlrqltouzqqpvpzzlzstytypnstlronp|a6dbb3d390f01789f34ca279f3ee4ac0df6ceacd|wlrqltou eli.jambu@gmail.com 2024-08-18 16:04:45 a6dbb3d3
├─╯  (no description set)$$--END--$$
◆  $$--START--$$|rwovpxktnwvyzpwmqsymtsquspvszwnl|235cdeaa5ef71894eca562a04eaee8d010ebe276|rwovpxkt eli.jambu@gmail.com 2024-08-18 15:07:25 235cdeaa
│  filterable selcteion box styling$$--END--$$
~  (elided revisions)
│ ○  $$--START--$$|vzpuwsqtotlxpkxpqqkxzzlrkupknztq|a26efab4741c026b298098bd4ef6f251b9c29945|vzpuwsqt eli.jambu@gmail.com 2024-08-18 11:56:20 a26efab4
├─╯  (empty) ss$$--END--$$
◆  $$--START--$$|zyonzlkqvopymszlpsztlrxqwttyxqoy|5cf5114617d86a60abe3dc33b77ff1c13ddcc202|zyonzlkq eli.jambu@gmail.com 2024-08-09 11:44:33 5cf51146
│  Replace tabs with 4 spaces becasue they cause issuse for nottui$$--END--$$
~|}
;;

let%expect_test "revs_graph_parsing" =
  let graph, ids = find_selectable_from_graph test_data_3 in
  let ids = ids |> Array.of_list in
  let ids_idx = ref 0 in
  graph
  |> Array.iter (fun x ->
    match x with
    | `Filler x ->
      "F:" |> print_endline;
      x |> print_endline
    | `Selectable x ->
      "S:" |> print_endline;
      let id = ids.(!ids_idx) in
      (match id with
       | Unique id ->
         id.change_id |> print_endline;
         id.commit_id |> print_endline
       | Duplicate id ->
         id.change_id |> print_endline;
         id.commit_id |> print_endline);
      incr ids_idx;
      x |> print_endline);
  [%expect
    {|
    S:
    zqtxnkuuryqzzolyksrylpzotmplmvus
    8ee443e4a374f7dfdd00494d8bf71af6162a1300
    @  zqtxnkuu eli.jambu@gmail.com 2025-02-15 21:22:48 8ee443e4
    │  (no description set)
    S:
    wmrnukwqvmnrsovsyxnmpwlkklnzpvpp
    b72673dcf464650064cdd80233b47848503cd01a
    ◌  wmrnukwq eli.jambu@gmail.com 2025-02-12 20:20:28 git_head() b72673dc
    │  wip: test
    S:
    oxpkqyozkvtxoklqutxztsmznxvwroxx
    9c3af61798927da4c80caa216688ea9d69e0d8bb
    ○  oxpkqyoz eli.jambu@gmail.com 2024-11-26 12:13:41 9c3af617
    │  added duplicate and undo commands
    S:
    tmyqqryzkukzztrpxrsxlqlzutptsnsw
    47d6c9a1db9d3756549dc7a68690f3df5600c83a
    ○  tmyqqryz eli.jambu@gmail.com 2024-11-23 21:50:19 47d6c9a1
    │  make update view fully async
    S:
    xwxrzsrzzlttpmysywysmnvtmrvysrvy
    8e338e5c76cdcfb71951367ff87eb1df98561fbd
    │ ○  xwxrzsrz eli.jambu@gmail.com 2024-11-23 21:17:50 8e338e5c
    │ │  use picos nottui
    S:
    osuupotwtypnmvnkrrlxmyxukvykmtsn
    3c203669a359b27833d9104c80505afd89af5f4d
    │ ○  osuupotw eli.jambu@gmail.com 2024-11-23 19:25:59 3c203669
    │ │  support await_read for async integration with nottui
    S:
    zmxzlvmwmnqruvrosmmknumpupoztuum
    668152f47d627e80184b8dbb5f40dd2b83ad6488
    │ ◌  zmxzlvmw eli.jambu@gmail.com 2024-11-11 12:06:45 668152f4
    ├─╯  wip: try to allow nottui to support picos
    S:
    opytqrnrrxlkzsxtkuvtvxyrpqsmrznl
    e9d81817d5e56e0817b725e2965dd36d3918a087
    │ ○  opytqrnr eli.jambu@gmail.com 2024-11-02 12:31:37 e9d81817
    ├─╯  safeguard obj.magic functions
    S:
    ozotxprmvvwvwuulxlokrmlksxtzpmmv
    e882ff4a65c0eea778c4d0cc572d63e85444bc93
    │ ○  ozotxprm eli.jambu@gmail.com 2024-11-01 21:03:37 test-issues* e882ff4a
    ├─╯  spooky testing branch indicator
    S:
    kyzmstkmrsnrvtzzpwwnummnzpwlousx
    15e7195328f95c7158dd1adc5e4bb50c7dd3372a
    ◆  kyzmstkm eli.jambu@gmail.com 2024-11-01 21:03:37 master v0.8.8 v0.8.9 15e71953
    │  fix remaining references to branch rather than bookmark
    F:
    ~  (elided revisions)

    S:
    uzuylryqmsmrlyzunluznwlkqsuurktp
    811f78b9f5d3272ff65a80d973ad9fe3db338fc8
    │ ○  uzuylryq eli.jambu@gmail.com 2024-10-31 18:49:04 811f78b9
    ├─╯  Try to make new dune build the project
    S:
    oorzkzkwlkqpptmnzvvqvkmxxxrvxpnv
    93c69eccd3e0838ee45946dc2b0eadbe4e679362
    ◆  oorzkzkw eli.jambu@gmail.com 2024-10-27 18:44:00 aaa v0.8.7 93c69ecc
    │  updated to use bookmark instead of branch
    F:
    ~  (elided revisions)

    S:
    nyzlmxtpvrxsormrrtzkwsopzuypppwr
    81fca5ab7626736be4f61323ca8f27ed35659343
    │ ○  nyzlmxtp eli.jambu@gmail.com 2024-09-30 21:13:28 81fca5ab
    ├─╯  debugging config
    S:
    llxznmqxrmtumwyprntkvzupkywpvwoz
    a529037b79b469d3c63857ef70395be46a38dda7
    ◆  llxznmqx eli.jambu@gmail.com 2024-09-30 20:57:49 a529037b
    │  multi-select
    F:
    ~  (elided revisions)

    S:
    wlrqltouzqqpvpzzlzstytypnstlronp
    a6dbb3d390f01789f34ca279f3ee4ac0df6ceacd
    │ ○  wlrqltou eli.jambu@gmail.com 2024-08-18 16:04:45 a6dbb3d3
    ├─╯  (no description set)
    S:
    rwovpxktnwvyzpwmqsymtsquspvszwnl
    235cdeaa5ef71894eca562a04eaee8d010ebe276
    ◆  rwovpxkt eli.jambu@gmail.com 2024-08-18 15:07:25 235cdeaa
    │  filterable selcteion box styling
    F:
    ~  (elided revisions)

    S:
    vzpuwsqtotlxpkxpqqkxzzlrkupknztq
    a26efab4741c026b298098bd4ef6f251b9c29945
    │ ○  vzpuwsqt eli.jambu@gmail.com 2024-08-18 11:56:20 a26efab4
    ├─╯  (empty) ss
    S:
    zyonzlkqvopymszlpsztlrxqwttyxqoy
    5cf5114617d86a60abe3dc33b77ff1c13ddcc202
    ◆  zyonzlkq eli.jambu@gmail.com 2024-08-09 11:44:33 5cf51146
    │  Replace tabs with 4 spaces becasue they cause issuse for nottui
    F:
    ~
    |}]
;;

let%expect_test "remove_ansi" =
  let str = "[31mHello[0m" in
  let str = remove_ansi str in
  str |> print_endline;
  [%expect {|Hello|}]
;;

let%expect_test "count_ansi" =
  let str = "[31mHello[0m" in
  let count = count_ansi str in
  count |> string_of_int |> print_endline;
  [%expect {|2|}]
;;
