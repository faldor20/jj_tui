module Key_Map = struct
  include Map.Make ( Key)

  let pp inner_pp fmt this =
    Format.fprintf fmt "@[<v>";
    this |> iter (fun k v -> Format.fprintf fmt "@[<h>%a@ %a@]" Key.pp k inner_pp v);
    Format.fprintf fmt "@]"
  ;;

  let show inner_pp this =
    pp inner_pp Format.str_formatter this;
    Format.flush_str_formatter ()
  ;;
end

type command = { command : string } [@@deriving show]

type sub_menu = {
    title : string
  ; subcommands : key_map
}

and key_map_item =
  | Sub_menu of sub_menu
  | Command of command

and key_map = key_map_item Key_Map.t[@@deriving show]
and key_map_update_t = key_map

let ( let* ) = Base.Result.Let_syntax.( >>= )
let ( let+ ) = Base.Result.Let_syntax.( >>| )

let rec key_map_item_of_yaml (value : string * Yaml.value) =
  match value with
  | key, `O [ ("sub", `O sub); ("title", `String title) ]
  | key, `O [ ("title", `String title); ("sub", `O sub) ] ->
    let* sub_items =
      List.map key_map_item_of_yaml sub
      |> Base.Result.all
      |> Result.map (fun x -> Key_Map.of_seq (List.to_seq x))
      |> Result.map_error (fun (`Msg msg) -> `Msg ("Invalid submenu: " ^ msg))
    in
    let+ key =
      Key.key_of_string key |> Result.map_error (fun msg -> `Msg ("Invalid key: " ^ msg))
    in
    key, Sub_menu { title; subcommands = sub_items }
  | key, `String command ->
    let+ key =
      Key.key_of_string key |> Result.map_error (fun msg -> `Msg ("Invalid key: " ^ msg))
    in
    key, Command { command }
  | _ ->
    Error (`Msg "Invalid YAML structure")
;;

let key_map_of_yaml (yaml : Yaml.value) =
  match yaml with
  | `O top_level ->
    List.map key_map_item_of_yaml top_level
    |> Base.Result.all
    |> Result.map (fun x -> Key_Map.of_seq (List.to_seq x))
  | _ ->
    Error (`Msg "Invalid YAML structure")
;;

let key_map_of_yaml_exn yaml =
  match key_map_of_yaml yaml with
  | Ok key_map ->
    key_map
  | Error (`Msg msg) ->
    failwith ("Invalid YAML structure: " ^ msg)
;;

open Yaml.Util

let rec key_map_item_to_yaml = function
  | key, Sub_menu { title; subcommands } ->
    let sub_yaml = key_map_to_yaml subcommands in
    let key = Key.key_to_string key in
    key, obj [ "title", string title; "sub", sub_yaml ]
  | key, Command { command } ->
    let key = Key.key_to_string key in
    key, string command

and key_map_to_yaml (key_map : key_map) : Yaml.value =
  obj (key_map |> Key_Map.to_seq |> Seq.map key_map_item_to_yaml |> List.of_seq)
;;

let key_map_update_t_to_yaml (key_map : key_map_update_t) : Yaml.value =
  key_map_to_yaml key_map
;;

let key_map_update_t_of_yaml (yaml : Yaml.value) = key_map_of_yaml yaml

(* let rec key_map_merge (key_map1 : key_map) (key_map2 : key_map) : key_map =
  let merged = Key_Map.create (Key_Map.length key_map1 + Key_Map.length key_map2) in
  Key_Map.iter (fun k v -> Key_Map.add merged k v) key_map1;
  Key_Map.iter
    (fun k v ->
      (**Find if the key is already in the merged map*)
      match Key_Map.find_opt merged k with
      | Some (Sub_menu { title = t1; subcommands = s1 }) ->
        (match v with
         | Sub_menu { title = t2; subcommands = s2 } ->
          let merged_subcommands = key_map_merge s1 s2 in
            Key_Map.replace
              merged
              k
              (Sub_menu { title = t2; subcommands = merged_subcommands })
         | Command _ ->
           Key_Map.replace merged k v)
      | Some (Command _) | None -> Key_Map.replace merged k v)
    key_map2;
  merged
;; *)

(**Merge two key maps, checking for duplicate keys*)
let key_map_apply_update override og =
  Key_Map.merge
    (fun k v1 v2 ->
      match v1, v2 with
      | Some og, Some override -> Some (override)
      | Some v, None | None, Some v -> Some v
      | None, None -> None)
    og
    override
;;

let cmd key id =
  let key = Key.key_of_string_exn key in
  key, Command { command = id }
;;

let sub key title sub =
  let key = Key.key_of_string_exn key in
  key, Sub_menu { title; subcommands = sub |> List.to_seq |> Key_Map.of_seq }
;;

let k_map list = list |> List.to_seq |> Key_Map.of_seq

type key_config = {
    global : key_map [@updater]
  ; graph : key_map [@updater]
  ; file : key_map [@updater]
}
[@@deriving yaml, record_updater ~derive:yaml]

(* Default key bindings matching current implementation *)
let default : key_config =
  let open Key in
  {
    global =
      k_map
        [ cmd "y" "confirm"; cmd "n" "decline"; cmd "h" "left_alt"; cmd "l" "right_alt" ]
  ; graph =
      k_map
        [
          sub "?" "Help" [ cmd "?" "show_help" ]
        ; cmd "?" "show_help"
        ; cmd "P" "prev"
        ; sub
            "n"
            "New Child"
            [
              cmd "n" "new_base"
            ; cmd "N" "new_no_edit"
            ; cmd "i" "new_inline"
            ; cmd "I" "new_inline_no_edit"
            ; cmd "p" "new_before"
            ; cmd "P" "new_before_no_edit"
            ]
        ; cmd "y" "duplicate"
        ; cmd "u" "undo"
        ; sub "c" "Commit" [ cmd "c" "commit_base"; cmd "C" "commit_no_edit"; cmd "D" "describe_editor" ]
        ; cmd "S" "split"
        ; sub
            "s"
            "Squash"
            [
              cmd "s" "squash_into_parent"
            ; cmd "S" "squash_into_rev"
            ; cmd "u" "squash_unsquash"
            ; cmd "i" "squash_interactive_parent"
            ; cmd "I" "squash_interactive_rev"
            ]
        ; cmd "e" "edit"
        ; cmd "d" "describe"
        ; cmd "D" "describe_editor"
        ; cmd "R" "resolve"
        ; sub
            "r"
            "Rebase"
            [ cmd "r" "rebase_single"; cmd "s" "rebase_with_descendants"; cmd "b" "rebase_with_bookmark" ]
        ; sub "g" "Git" [ cmd "p" "git_push"; cmd "f" "git_fetch"; cmd "F" "git_fetch_all" ]
        ; cmd "z" "parallelize"
        ; cmd "a" "abandon"
        ; sub
            "b"
            "Bookmark"
            [
              cmd "c" "bookmark_create"
            ; cmd "d" "bookmark_delete"
            ; cmd "f" "bookmark_forget"
            ; cmd "r" "bookmark_rename"
            ; cmd "s" "bookmark_set"
            ; cmd "t" "bookmark_track"
            ; cmd "u" "bookmark_untrack"
            ]
        ; cmd "f" "filter"
        ; cmd "A" "absorb"
        ]
  ; file =
      k_map
        [
          cmd "?" "show_help"
        ; cmd "m" "move_to_rev"
        ; cmd "N" "move_to_child"
        ; cmd "P" "move_to_parent"
        ; cmd "a" "abandon"
        ; sub
            "A"
            "absorb"
            [
             cmd "a" "absorb"
            ; cmd "t" "absorb-into"
            ]
        ; cmd "u" "undo"
        ]
  }
;;

(* Example usage:
   key_of_string "C+S+A+a" = Ok { key = 'a'; modifiers = [`Ctrl; `Shift; `Meta] }
   key_of_string "C+x" = Ok { key = 'x'; modifiers = [`Ctrl] }
   key_of_string "a" = Ok { key = 'a'; modifiers = [] }
   key_of_string "C+S+" = Error "No key character provided"
   key_of_string "X+a" = Error "Unknown modifier: X"
*)

let sample =
  {|
  c:
    title: "Commit"
    sub:
      a: amend
      n: new
      s: 
        sub:
          p: into_parent
          r: into_rev
          u: unsquash
          C+r: interactive_parent
          C+i: interactive_rev
        title: "Squash"
|}
;;

let%expect_test "parse yaml" =
  let yaml = Yaml.of_string_exn sample in
  let key_map = key_map_of_yaml_exn yaml in
  print_endline (Yaml.to_string_exn (key_map_to_yaml key_map));
  [%expect
    {|
    c:
      title: Commit
      sub:
        a: amend
        "n": new
        s:
          title: Squash
          sub:
            C+i: interactive_rev
            p: into_parent
            r: into_rev
            C+r: interactive_parent
            u: unsquash
    |}]
;;

let%expect_test "merge" =
  let yaml = Yaml.of_string_exn sample in
  let key_map = key_map_of_yaml_exn yaml in
  let overrides =
    k_map [ cmd "c" "override"; sub "s" "Squash" [ cmd "c" "override2" ] ]
  in
  let merged = key_map_apply_update overrides key_map in
  print_endline (Yaml.to_string_exn (key_map_to_yaml merged));
  [%expect
    {|
    c: override
    s:
      title: Squash
      sub:
        c: override2
    |}]
;;
