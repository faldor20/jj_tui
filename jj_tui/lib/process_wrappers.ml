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

(** Matches any basic ansi escape codes*)
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

(** Removes any found ansi escape codes*)
let remove_ansi str = str |> Re.replace_string ~by:"" ansi_regex

let count_ansi str = str |> Re.all ansi_regex |> List.length

let find_selectable_from_graph limit str =
  (* Matches  a single revision in the format specificied by the graph template  *)
  let matches =
    str
    |> Re.split_full
         (Re.Pcre.regexp
            ~flags:[ Re.Pcre.(`MULTILINE) ]
            {|(^.*?)\$\$--START--\$\$\|(.+?)\|(.+?)\|(.+?)\|(.+?)\|([\s\S]*?)\$\$--END--\$\$\n?|})
  in
  let selectable_count = ref 0 in
  let graph_rev, ids_rev =
    matches
    |> List.fold_left
         (fun (graph_acc, ids_acc) chunk ->
            match chunk with
            | `Delim selectable ->
              let graph_bit = Re.Group.get selectable 1 in
              (* In future we should be able to use the strifify function in ocaml*)
              let change_id = Re.Group.get selectable 2 |> remove_ansi in
              let commit_id = Re.Group.get selectable 3 |> remove_ansi in
              let divergent =
                match Re.Group.get selectable 4 |> remove_ansi with
                | "true" ->
                  true
                | "false" ->
                  false
                | content ->
                  failwith @@ "Couldn't parse jj divergent value:" ^ content
              in
              let hidden =
                match Re.Group.get selectable 5 |> remove_ansi with
                | "true" ->
                  true
                | "false" ->
                  false
                | content ->
                  failwith @@ "Couldn't parse jj divergent value:" ^ content
              in
              [%log
                debug
                  "parsed rev: change_id: %s commit_id: %s divergent: %s"
                  change_id
                  commit_id
                  (Re.Group.get selectable 6 |> remove_ansi)];
              let rev = { commit_id; change_id; divergent } in
              let id =
                if divergent || hidden then Duplicate commit_id else Unique change_id
              in
              let content = Re.Group.get selectable 6 in
              incr selectable_count;
              `Selectable (graph_bit ^ content) :: graph_acc, id :: ids_acc
            | `Text filler ->
              (*Anything between our match is non-selectable filler*)
              if filler = ""
              then graph_acc, ids_acc
              else `Filler filler :: graph_acc, ids_acc)
         ([], [])
  in
  let graph =
    (if !selectable_count >= (limit)
     then (
  [%log debug "limit: %d selectable: %d" limit !selectable_count];
       let txt =
         Printf.sprintf
           "\nHit limit of %d items.\nIncrease limit in config or make your revset more \
            precise\n"
           limit
       in
       `Filler txt :: graph_rev)
     else graph_rev)
    |> List.rev
    |> Array.of_list
  in
  graph, ids_rev |> List.rev
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

  (* Currently hard-coded. Soon it'l be settable in config *)
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

  let graph_info_template node_template =
    {|"$$--START--$$"++"|"++change_id++"|"++commit_id++"|"++divergent++"|"++hidden++"|"++|}
    ^ node_template
    ^ {|++"$$--END--$$"++""|}
  ;;

  let get_graph_info node_template revset_arg limit =
    let output =
      jj_no_log ([ "log"; "-T"; graph_info_template node_template;"--limit"; limit |>string_of_int] @ revset_arg)
    in
    output |> find_selectable_from_graph limit
  ;;

  (** returns the graph and a list of revs within that graph*)
  let graph_and_revs ?revset limit () =
    (*We join_after here to ensure any errors in sub-fibers only propegate to here, otherwise fibers everywhere would get cancelled when an error here occurs*)
    Flock.join_after @@ fun _ ->
    let graph =
      Flock.fork_as_promise @@ fun () ->
      let revset_arg = match revset with Some revset -> [ "-r"; revset ] | None -> [] in
      get_graph_info base_graph_template revset_arg limit
    in
    let graph, revs = Promise.await graph in
    graph, revs |> Array.of_list
  ;;
end

(*========Tests======*)

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
  let graph, ids = find_selectable_from_graph 2000 test_data_3  in
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
         (* id.change_id |> print_endline; *)
         (* id.commit_id |> print_endline *)
         id |> print_endline
       | Duplicate id ->
         (* id.change_id |> print_endline; *)
         (* id.commit_id |> print_endline); *)
         id |> print_endline);
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
