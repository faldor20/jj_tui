type modifier = [ `Meta | `Shift | `Ctrl ]

type key = {
  key: char;
  modifiers: modifier list;
}

let sort_and_dedup_modifiers mods =
  let modifier_order = function
    | `Shift -> 0
    | `Meta -> 1
    | `Ctrl -> 2
  in
  mods
  |> List.sort_uniq (fun a b -> compare (modifier_order a) (modifier_order b))

let key_of_string str =
  let parts = String.split_on_char '+' str in
  let rec process_parts mods = function
    | [] -> Error "No key character provided"
    | [k] when String.length k = 1 ->
        let key = k.[0] in
        Ok { key = key; modifiers = sort_and_dedup_modifiers ( mods) }
    | mod_str :: rest ->
      let modifier = match String.uppercase_ascii mod_str with
        | "C" | "CTRL" -> Ok `Ctrl
        | "S" | "SHIFT" -> Ok `Shift
        | "A" | "ALT" -> Ok `Meta
        | other -> Error (Printf.sprintf "Unknown modifier: %s" other)
      in
      match modifier with
      | Ok m -> process_parts (m :: mods) rest
      | Error e -> Error e
  in
  process_parts [] parts

let key_of_string_exn str= key_of_string str|>Result.get_ok

let key_to_string { key; modifiers } =
  let modifier_str =
    modifiers
    |> List.map (function
      | `Shift -> "S"
      | `Meta -> "A"
      | `Ctrl -> "C")
    |> String.concat "+"
  in
  if modifier_str = "" then
    String.make 1 key
  else
    modifier_str ^ "+" ^ (String.make 1 key)

let key_of_yaml = function
  | `String s ->
    (match key_of_string s with
     | Ok k -> Ok k
     | Error msg -> Error (`Msg("Invalid key format: " ^ msg)))
  | _ -> Error (`Msg "Expected string for key")

let key_to_yaml k =
  `String (key_to_string k)

let pp_key fmt k = Format.fprintf fmt "%s" (key_to_string k)
(* Update all the types to use key instead of char *)
type bookmark_keys = {
  menu:key;
  create : key;
  delete : key;
  forget : key;
  rename : key;
  set : key;
  track : key;
  untrack : key;
}[@@deriving yaml, record_updater ~derive: yaml]

type git_keys = {
  menu:key;
  push : key;
  fetch : key;
}[@@deriving yaml, record_updater ~derive: yaml]

type squash_keys = {
  menu:key;
  into_parent : key;
  into_rev : key;
  unsquash : key;
  interactive_parent : key;
  interactive_rev : key;
}[@@deriving yaml, record_updater ~derive: yaml]

type rebase_keys = {
  menu:key;
  single : key;
  with_descendants : key;
  with_bookmark : key;
}[@@deriving yaml, record_updater ~derive: yaml]

type file_keys = {
  show_help : key;
  move_to_rev : key;
  move_to_child : key;
  move_to_parent : key;
  discard : key;
}[@@deriving yaml, record_updater ~derive: yaml]

type new_child_keys= {
  menu:key;
  base:key;
  no_edit:key;
  inline:key;
  inline_no_edit:key;
}[@@deriving yaml, record_updater ~derive: yaml]

type commit_keys = {
  menu:key;
  base:key;
  no_edit:key;
  open_editor:key;
}[@@deriving yaml, record_updater ~derive: yaml]

type graph_keys = {
  show_help : key;
  prev : key;
  new_child : new_child_keys; [@updater]
  duplicate : key;
  undo : key;
  commit : commit_keys;[@updater]
  split : key;
  squash : squash_keys;[@updater]
  edit : key;
  describe : key;
  describe_editor : key;
  resolve : key;
  rebase : rebase_keys;[@updater]
  git : git_keys;[@updater]
  parallelize : key;
  abandon : key;
  bookmark : bookmark_keys;[@updater]
  filter : key;
}[@@deriving yaml, record_updater ~derive: yaml]

type t = {
  graph : graph_keys; [@updater]
  file : file_keys;[@updater]
}[@@deriving yaml, record_updater ~derive: yaml]

(* Helper to create a simple key without modifiers *)
let simple_key c = { key = c; modifiers = [] }

(* Default key bindings matching current implementation *)
let default:t = {
  graph = {
    show_help = simple_key '?';
    prev = simple_key 'P';

    new_child={
      menu= simple_key 'n';
      base= simple_key 'n';
      no_edit= key_of_string_exn "N";
      inline= simple_key 'i';
      inline_no_edit= simple_key 'I';
    };
    duplicate = simple_key 'y';
    undo = simple_key 'u';
    commit = {
      menu = simple_key 'c';
      base = simple_key 'c';
      no_edit = simple_key 'C';
      open_editor = simple_key 'D';
    };
    split = simple_key 'S';
    squash = {
      menu = simple_key 's';
      into_parent = simple_key 's';
      into_rev = simple_key 'S';
      unsquash = simple_key 'u';
      interactive_parent = simple_key 'i';
      interactive_rev = simple_key 'I';
    };
    edit = simple_key 'e';
    describe = simple_key 'd';
    describe_editor = simple_key 'D';
    resolve = simple_key 'R';
    rebase = {
      menu = simple_key 'r';
      single = simple_key 'r';
      with_descendants = simple_key 's';
      with_bookmark = simple_key 'b';
    };
    git = {
      menu = simple_key 'g';
      push = simple_key 'p';
      fetch = simple_key 'f';
    };
    parallelize = simple_key 'z';
    abandon = simple_key 'a';
    bookmark = {
      menu = simple_key 'b';
      create = simple_key 'c';
      delete = simple_key 'd';
      forget = simple_key 'f';
      rename = simple_key 'r';
      set = simple_key 's';
      track = simple_key 't';
      untrack = simple_key 'u';
    };
    filter = simple_key 'f';
  };
  file = {
    show_help = simple_key '?';
    move_to_rev = simple_key 'm';
    move_to_child = simple_key 'N';
    move_to_parent = simple_key 'P';
    discard = simple_key 'd';
  };
}

(* Example usage:
   key_of_string "C+S+A+a" = Ok { key = 'a'; modifiers = [`Ctrl; `Shift; `Meta] }
   key_of_string "C+x" = Ok { key = 'x'; modifiers = [`Ctrl] }
   key_of_string "a" = Ok { key = 'a'; modifiers = [] }
   key_of_string "C+S+" = Error "No key character provided"
   key_of_string "X+a" = Error "Unknown modifier: X"
*)
