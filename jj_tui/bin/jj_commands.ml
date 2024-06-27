(** The JJ_commands module defines all the tools we need to create and execute keymappings
    It allows us to define a command list: A list of keys, commands and descriptions
    We can then run a command matching a key or generate a documentation UI element showing all available commands *)

(** Internal to this module. I'm trying this out as a way to avoid .mli files*)
module Shared = struct
  type cmd_args = string list

  (** Regular jj command *)
  type command_variant =
    | Cmd of cmd_args (** Regular jj command *)
    | Cmd_r of cmd_args
    (** Regular jj command that should operate on the selected revison *)
    | Dynamic of (unit -> command_variant)
    | Dynamic_r of (string -> command_variant)
    (** Wraps a command so that the content will be regenerated each time it's run. Usefull if you wish to read some peice of ui state *)
    | Cmd_I of cmd_args
    (** Command that will open interactively. Used for diff editing to hand control over to the jj process *)
    | Prompt of string * cmd_args
    | Prompt_r of string * cmd_args
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
  open Shared
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Nottui
  open! Jj_tui.Util

  exception Handled

  let render_command_line ~indent_level key desc =
    let indent = String.init (indent_level * 2) (fun _ -> ' ') in
    I.hcat
      [
        I.string A.empty indent
      ; I.uchars (A.fg A.lightblue) key
      ; I.strf " "
      ; desc |> String.split_on_char '\n' |> List.map (I.string A.empty) |> I.vcat
      ]
  ;;

  let rec render_commands ?(indent_level = 0) commands =
    commands
    |> List.concat_map @@ fun command ->
       match command with
       | {
         key
       ; description
       ; cmd =
           ( Cmd _
           | Cmd_I _
           | Prompt _
           | Prompt_I _
           | Fun _
           | PromptThen _
           | Dynamic _
           | Cmd_r _
           | Prompt_r _
           | Dynamic_r _ )
       } ->
         [ render_command_line ~indent_level [| key |> Uchar.of_char |] description ]
       | { key; description; cmd = SubCmd subs } ->
         render_command_line ~indent_level [| key |> Uchar.of_char |] description
         :: render_commands ~indent_level:(indent_level + 1) subs
  ;;

  let commands_list_ui commands =
    let move_command =
      render_command_line
        ~indent_level:0
        ("Alt+Arrows" |> String.to_seq |> Seq.map Uchar.of_char |> Array.of_seq)
        "navigation between windows"
    in
    (commands |> render_commands) @ [ move_command ]
    |> I.vcat
    |> Ui.atom
    |> Lwd.pure
    |> W.Scroll.area
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
           W.Overlay.
             {
               label = str
             ; pre_fill = ""
             ; on_exit =
                 (function
                   | `Finished str ->
                     safe_jj (fun _ ->
                       match cmd with
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
                     ())
             }
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
    | Cmd_r args ->
      ui_state.show_popup $= None;
      noOut (args @ [ "-r"; Vars.get_selected_rev () ]);
      raise Handled
    | Prompt (str, args) ->
      ui_state.show_popup $= None;
      prompt str (`Cmd args);
      raise Handled
    | Prompt_r (str, args) ->
      ui_state.show_popup $= None;
      prompt str (`Cmd (args @ [ "-r"; Vars.get_selected_rev () ]));
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
    | Dynamic_r f ->
      f (Vars.get_selected_rev ()) |> handleCommand description

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
    | JJError (cmd, error) ->
      handle_jj_error cmd error;
      `Unhandled

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
  open! Jj_tui.Util
  open Intern (Vars)
  include Shared

  (** A handy command_list that just has this help command for areas that don't have any commands to still show help*)
  let rec default_list =
    [
      {
        key = 'h'
      ; description = "Show help"
      ; cmd =
          Fun
            (fun _ ->
              ui_state.show_popup $= Some (commands_list_ui default_list, "Help");
              ui_state.input $= `Mode (fun _ -> `Unhandled))
      }
    ]
  ;;

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
