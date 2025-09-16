type modifier = [ `Meta | `Shift | `Ctrl ]

type t = {
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

let key_of_string_exn str= match key_of_string str with Ok k -> k | Error msg -> failwith ("Invalid key: " ^ msg)

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

let pp fmt k = Format.fprintf fmt "%s" (key_to_string k)

let equal k1 k2 =  k1.key =  k2.key && k1.modifiers = k2.modifiers

let hash k = Char.hash k.key + List.fold_left (fun acc m -> acc * 31 + match m with
  | `Meta -> 1
  | `Shift -> 2
  | `Ctrl -> 3
) 0 k.modifiers

let compare k1 k2 =
  match compare k1.key k2.key with
  | 0 -> List.compare compare k1.modifiers k2.modifiers
  | c -> c
;;
