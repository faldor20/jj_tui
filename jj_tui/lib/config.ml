open Util
open Logging

type t = { key_map : Key_map.t[@updater] } [@@deriving yaml, record_updater ~derive: yaml]


let default_config:t =
  {
    key_map= Key_map.default
  }
;;

let get_config_dir () =
  let os = Os.poll_os () in
  let config_home =
    match os with
    | Some "linux" ->
      Sys.getenv_opt "XDG_CONFIG_HOME" |> Option.value ~default:"~/.config"
    | Some "macos" ->
      let home = Unix.getenv "HOME" in
      Filename.concat home "Library/Preferences"
    | _ ->
      Sys.getenv_opt "HOME"
      |> Option.map (Filename.concat "config")
      |> Option.value ~default:"./config"
  in
  Filename.concat config_home "jj_tui"
;;


let load_config () =
  [%log info "Loading config..."];
  let config_file = Filename.concat (get_config_dir ()) "config.json" in
  try
    let ic = open_in config_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let json = Yaml.of_string_exn content in
    match t_update_t_of_yaml json with
    | Ok (config_update) ->
      [%log info "Config loaded!"];
      default_config |> t_apply_update config_update 
    | Error (`Msg msg) ->
      [%log warn "Error parsing config: %s" msg];
      default_config
  with
  | Sys_error _ ->
    [%log info "No config file found at %s, using defaults" config_file];
    default_config
  | ex ->
    [%log warn "Error loading config: %s" (Printexc.to_string ex)];
    default_config
;;
