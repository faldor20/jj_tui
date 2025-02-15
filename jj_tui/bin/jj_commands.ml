(** The JJ_commands module defines all the tools we need to create and execute keymappings
    It allows us to define a command list: A list of keys, commands and descriptions
    We can then run a command matching a key or generate a documentation UI element showing all available commands *)

open Jj_tui.Logging

(** Internal to this module. I'm trying this out as a way to avoid .mli files*)
module Shared = struct
  type cmd_args = string list [@@deriving show]

  type 'a revision_type =
    | Hovered of 'a
    | Selected of 'a
    | Active of 'a (** Regular jj command *)
  [@@deriving show]

  type 'a command_variant =
    | Cmd of cmd_args (** Regular jj command *)
    | Cmd_r of cmd_args
    (** Regular jj command that should operate on the hovered revison *)
    | Cmd_with_revs of cmd_args revision_type
    (** Regular jj command that should operate on active revisions*)
    | Dynamic of (unit -> 'a command_variant)
    | Dynamic_r of (string -> 'a command_variant)
    (** Wraps a command so that the content will be regenerated each time it's run. Usefull if you wish to read some peice of ui state *)
    | Cmd_I of cmd_args
    (** Command that will open interactively. Used for diff editing to hand control over to the jj process *)
    | Prompt of string * cmd_args
    | Selection_prompt of
        string
        * (unit -> 'a Nottui.W.Lists.multi_selectable_item list Lwd.t)
        * (string -> 'a -> bool)
        * ('a -> 'a command_variant)
    | Prompt_r of string * cmd_args
    (** Creates a prompt and then runs the command with the prompt result appended as the last arg *)
    | PromptThen of string * (string -> 'a command_variant)
    (** Same as prompt except you can run another command after. Useful if you want multiple prompts *)
    | Prompt_I of string * cmd_args
    (** Same as prompt but expects the command to be interactive same as [Cmd_I] *)
    | SubCmd of 'a command list
    (** Allows nesting of commands, shows a popup with command options and waits for the user to press the appropriate key*)
    | Fun of (unit -> unit)
    (** Execute an arbitrary function. Prefer other command types if possible *)
  [@@deriving show]

  (** A command that should be run when it's key is pressed*)
  and 'a command = {
      key : char
    ; description : string
    ; cmd : 'a command_variant
  }
  [@@deriving show]
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

  let get_revs rev_type =
    match rev_type with
    | Hovered a ->
      a, [ get_hovered_rev () ]
    | Selected a ->
      a, get_selected_revs ()
    | Active a ->
      a, get_active_revs ()
  ;;

  (**resets the selection if there was a selection and the command revision type used it*)
  let reset_selection_post_cmd rev_type =
    match rev_type with
    | Selected _ ->
      Vars.reset_selection ()
    | Active _ ->
      if Vars.get_selected_revs () |> List.length > 0 then Vars.reset_selection ()
    | _ ->
      ()
  ;;

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
           | Selection_prompt _
           | Cmd_with_revs _
           | Cmd_r _
           | Prompt_r _
           | Dynamic_r _ )
       } ->
         [ render_command_line ~indent_level [| key |> Uchar.of_char |] description ]
       | { key; description; cmd = SubCmd subs } ->
         render_command_line ~indent_level [| key |> Uchar.of_char |] description
         :: render_commands ~indent_level:(indent_level + 1) subs
  ;;

  let commands_list_ui ?(include_arrows = false) commands =
    let move_command =
      render_command_line
        ~indent_level:0
        ("Arrows" |> String.to_seq |> Seq.map Uchar.of_char |> Array.of_seq)
        "navigation between windows"
    in
    ((commands |> render_commands) @ if include_arrows then [ move_command ] else [])
    |> I.vcat
    |> Ui.atom
    |> Lwd.pure
    |> W.Scroll.area
  ;;

  let rec handleCommand description cmd =
    [%log info "Handling command: %s" description];
    let noOut args =
      let _ = args in
      let _result = jj args in
      Global_funcs.update_status ~cause_snapshot:false ();
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
                         Global_funcs.update_status ~cause_snapshot:false ();
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
    let prompt_selection str items filter_predicate cmd =
      ui_state.show_string_selection_prompt
      $= Some
           W.Overlay.
             {
               items = items ()
             ; filter_predicate
             ; label = str
             ; on_exit =
                 (function
                   | `Finished x ->
                     safe_jj (fun _ -> cmd x |> command_no_input description)
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
      noOut (args @ [ "-r"; Vars.get_hovered_rev () ]);
      raise Handled
    | Cmd_with_revs rev_type ->
      let args, revs = get_revs rev_type in
      ui_state.show_popup $= None;
      noOut (args @ ("-r" :: revs));
      reset_selection_post_cmd rev_type;
      raise Handled
    | Prompt (str, args) ->
      ui_state.show_popup $= None;
      prompt str (`Cmd args);
      raise Handled
    | Prompt_r (str, args) ->
      ui_state.show_popup $= None;
      prompt str (`Cmd (args @ [ "-r"; Vars.get_hovered_rev () ]));
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
    | Selection_prompt (str, items, predicate, cmd) ->
      ui_state.show_popup $= None;
      ui_state.show_prompt $= None;
      prompt_selection str items predicate cmd;
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
      f (Vars.get_hovered_rev ()) |> handleCommand description

  (** Try mapching the command mapping to the provided key and run the command if it matches *)
  and command_input ~is_sub keymap key =
    (* Use exceptions so we can break out of the list*)
    let input = Lwd.peek ui_state.input in
    try
      keymap
      |> List.iter (fun cmd ->
        if cmd.key == key then handleCommand cmd.description cmd.cmd else ());
      `Unhandled
    with
    | Handled ->
      (*If this is a sub command and we didn't change to some other subcommand we should exit back to  normal command operation*)
      if is_sub && input == Lwd.peek ui_state.input then ui_state.input $= `Normal;
      `Handled
    | Jj_process.JJError (cmd, error) ->
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
        key = '?'
      ; description = "Show help"
      ; cmd =
          Fun
            (fun _ ->
              ui_state.show_popup
              $= Some (commands_list_ui ~include_arrows:true default_list, "Help");
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

  (** Handles raw command mapping without regard for modes or the current intput state. Should be used when setting a new input mode*)
  let command_input = command_input

  (** Handles input and sub_commands.*)
  let handleInputs command_mapping =
    match Lwd.peek ui_state.input with
    | `Mode mode ->
      mode
    | `Normal ->
      command_input ~is_sub:false command_mapping
  ;;
end
