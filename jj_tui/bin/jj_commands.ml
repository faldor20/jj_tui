(** The JJ_commands module defines all the tools we need to create and execute keymappings
    It allows us to define a command list: A list of keys, commands and descriptions
    We can then run a command matching a key or generate a documentation UI element showing all available commands *)

(** Internal to this module. I'm trying this out as a way to avoid .mli files*)
module Shared = struct
  type cmd_args = string list

  (** Regular jj command *)
  type command_variant =
    | Cmd of cmd_args (** Regular jj command *)
    | Dynamic of (unit -> command_variant)
    (** Wraps a command so that the content will be regenerated each time it's run. Usefull if you wish to read some peice of ui state *)
    | Cmd_I of cmd_args
    (** Command that will open interactively. Used for diff editing to hand control over to the jj process *)
    | Prompt of string * cmd_args
    (** Creates a prompt and then runs the command with the prompt result appended as the last arg *)
    | PromptThen of string * (string -> command_variant)
    (** Same as prompt except you can run another command after. Useful if you want multiple prompts *)
    | Prompt_I of string * cmd_args
    (** Same as prompt but expects the command to be interactive same as [Cmd_I] *)
    | SubCmd of command list
    (** Allows nesting of commands, shows a popup with command options and waits for the user to press the appropriate key*)
    | Fun of (unit -> unit)
    (** Execute an arbitrary function. Prefer other command types if possible *)

  (** A command that should be run when it's key is pressed*)
  and command = {
      key : char
    ; description : string
    ; cmd : command_variant
  }
end

(** Internal to this module. I'm trying this out as a way to avoid .mli files*)
module Intern (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Jj_tui
  module W = Nottui_widgets
  open Nottui
  open! Jj_tui.Util
  module Wd = Widgets
  open Shared

  exception Handled

  let rec render_commands ?(sub_level = 0) commands =
    let indent = String.init (sub_level * 2) (fun _ -> ' ') in
    let line key desc =
      I.hcat
        [
          I.string A.empty indent
        ; I.char (A.fg A.lightblue) key 1 1
        ; I.strf " "
        ; desc |> String.split_on_char '\n' |> List.map (I.string A.empty) |> I.vcat
        ]
    in
    commands
    |> List.concat_map @@ fun command ->
       match command with
       | {
         key
       ; description
       ; cmd = Cmd _ | Cmd_I _ | Prompt _ | Prompt_I _ | Fun _ | PromptThen _ | Dynamic _
       } ->
         [ line key description ]
       | { key; description; cmd = SubCmd subs } ->
         line key description :: render_commands ~sub_level:(sub_level + 1) subs
  ;;

  let commands_list_ui commands =
    commands |> render_commands |> I.vcat |> Ui.atom |> Lwd.pure |> Wd.scroll_area
  ;;

  let rec handleCommand description cmd =
    let noOut args =
      let _ = jj args in
      Global_funcs.update_status ();
      ()
    in
    let prompt str cmd =
      ui_state.show_prompt
      $= Some
           ( str
           , ""
           , function
             | `Finished str ->
               (match cmd with
                | `Cmd args ->
                  let _result = jj (args @ [ str ]) in
                  Global_funcs.update_status ();
                  ()
                  (* v_cmd_out $= jj (args @ [ str ]); *)
                | `Cmd_I args ->
                  Lwd.set ui_state.view (`Cmd_I (args @ [ str ]))
                | `Fun func ->
                  func str)
             | `Closed ->
               () )
    in
    let change_view view = Lwd.set ui_state.view view in
    let send_cmd args = change_view (`Cmd_I args) in
    match cmd with
    | Cmd_I args ->
      ui_state.show_popup $= None;
      send_cmd args;
      raise Handled
    | Cmd args ->
      ui_state.show_popup $= None;
      noOut args;
      raise Handled
    | Prompt (str, args) ->
      ui_state.show_popup $= None;
      prompt str (`Cmd args);
      raise Handled
    | PromptThen (label, next) ->
      ui_state.show_popup $= None;
      (*We run a prompt that then runs our next command when finished*)
      prompt label @@ `Fun (fun x -> next x |> command_no_input description);
      raise Handled
    | Prompt_I (str, args) ->
      ui_state.show_popup $= None;
      prompt str (`Cmd_I args);
      raise Handled
    | Fun func ->
      ui_state.show_popup $= None;
      func ();
      Global_funcs.update_status ();
      raise Handled
    | SubCmd sub_map ->
      ui_state.show_popup $= Some (commands_list_ui sub_map, description);
      ui_state.input $= `Mode (command_input ~is_sub:true sub_map);
      raise Handled
    | Dynamic f ->
      f () |> handleCommand description

  (** Try mapching the command mapping to the provided key and run the command if it matches *)
  and command_input ~is_sub keymap key =
    (* Use exceptions so we can break out of the list*)
    try
      keymap
      |> List.iter (fun cmd ->
        if cmd.key == key then handleCommand cmd.description cmd.cmd else ());
      `Unhandled
    with
    | Handled ->
      if is_sub then ui_state.input $= `Normal;
      `Handled

  and command_no_input description cmd =
    (* Use exceptions so we can break out of the list*)
    try
      handleCommand description cmd;
      ()
    with
    | Handled ->
      ()
  ;;
end

open Nottui

module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Jj_tui
  module W = Nottui_widgets
  open! Jj_tui.Util
  open Intern (Vars)
  module Wd = Widgets
  include Shared

  (**Generate a UI object with all the commands nicely formatted and layed out. Useful for help text*)
  let commands_list_ui = commands_list_ui

  (**`Prompt`:Allows running one command and then running another using the input of the first*)
  let confirm_prompt prompt cmd =
    SubCmd [ { key = 'y'; description = "Yes I want to " ^ prompt; cmd } ]
  ;;

  (** Handles input and sub_commands.*)
  let handleInputs command_mapping =
    match Lwd.peek ui_state.input with
    | `Mode mode ->
      mode
    | `Normal ->
      command_input ~is_sub:false command_mapping
  ;;
end
