module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  open Jj_tui
  module W = Nottui_widgets
  open Nottui
  open! Jj_tui.Util
  module Wd = Widgets

  type cmd_args = string list

  (**`Prompt`:Allows running one command and then running another using the input of the first*)
  type command_variant =
    | Cmd of cmd_args
    | Cmd_I of cmd_args
    | Prompt of string * cmd_args
    | PromptThen of string * (string -> command_variant)
    | Prompt_I of string * cmd_args
    | Prompt_Fn of string * (unit->cmd_args)
    | SubCmd of command list
    | Fun of (unit -> unit)

  and command = {
    key : char;
    description : string;
    cmd : command_variant;
  }

  type command_list = command list

  exception Handled

  let rec render_commands ?(sub_level = 0) commands =
    let indent = String.init (sub_level * 2) (fun _ -> ' ') in
    let line key desc =
      I.hcat
        [
          I.string A.empty indent;
          I.char (A.fg A.lightblue) key 1 1;
          I.strf " ";
          desc |> String.split_on_char '\n' |> List.map (I.string A.empty) |> I.vcat;
        ]
    in
    commands
    |> List.concat_map @@ fun command ->
       match command with
       | {
        key;
        description;
        cmd = Cmd _ | Cmd_I _ | Prompt _ | Prompt_I _ | Fun _ | PromptThen _| Prompt_Fn _;
       } ->
         [ line key description ]
       | { key; description; cmd = SubCmd subs } ->
         line key description :: render_commands ~sub_level:(sub_level + 1) subs
  ;;

  let commands_list_ui commands =
    commands |> render_commands |> I.vcat |> Ui.atom |> Lwd.pure |> Wd.scroll_area
  ;;

  let rec commandMapping =
    [
      {
        key = 'h';
        description = "Show help";
        cmd =
          Fun
            (fun _ ->
              ui_state.show_popup $= Some (commands_list_ui commandMapping, "Help");
              ui_state.input $= `Mode (fun _ -> `Unhandled));
      };
      {
        key = '5';
        description = "Show help2";
        cmd =
          SubCmd
            [
              {
                key = '1';
                description = "Show help2";
                cmd =
                  Fun
                    (fun _ ->
                      ui_state.show_popup $= Some (commands_list_ui commandMapping, "Help");
                      ui_state.input $= `Mode (fun _ -> `Unhandled));
              };
            ];
      };
      {
        key = 'P';
        description = "Move the working copy to the previous child ";
        cmd = Cmd [ "prev" ];
      };
      {
        key = 'p';
        description = "Edit the previous child change";
        cmd = Cmd [ "prev"; "--edit" ];
      };
      {
        key = 'N';
        description = "Move the working copy to the next child ";
        cmd = Cmd [ "next" ];
      };
      {
        key = 'n';
        description = "Edit the next child change";
        cmd = Cmd [ "next"; "--edit" ];
      };
      { key = 'i'; cmd = Cmd [ "new" ]; description = "Make a new empty change" };
      {
        key = 'c';
        description = "Describe this change and move on (same as `describe` then `new`) ";
        cmd = Prompt ("commit msg", [ "commit"; "-m" ]);
      };
      {
        key = 'S';
        description = "Split the current commit interacively";
        cmd = Cmd_I [ "split"; "-i" ];
      };
      {
        key = 's';
        description = "Squash/unsquash (has subcommands)";
        cmd =
          SubCmd
            [
              {
                key = 'S';
                cmd = Cmd_I [ "unsquash"; "-i" ];
                description = "Interactivaly unsquash";
              };
              {
                key = 's';
                description = "Squash into parent";
                cmd =
                  Fun
                    (fun _ ->
                      let curr_msg, prev_msg = get_messages () in
                      let new_msg = prev_msg ^ curr_msg in
                      jj [ "squash"; "--quiet"; "-m"; new_msg ] |> ignore);
              };
              {
                key = 'S';
                description = "Squash into any commit";
                cmd =
                  PromptThen
                    ( "target revision",
                      fun str ->
                        let curr_msg, prev_msg = get_messages () in
                        let new_msg = prev_msg ^ curr_msg in
                        Cmd [ "squash"; "--quiet"; "-m"; new_msg; "--into"; str ] );
              };
              {
                key = 'i';
                description = "Interactively choose what to squash into parent";
                cmd = Cmd_I [ "squash"; "-i" ];
              };
              {
                key = 'I';
                description = "Interactively choose what to squash into a commit";
                cmd = Prompt_I ("target revision", [ "squash"; "-i"; "--into" ]);
              };
            ];
      };
      {
        key = 'e';
        cmd = Prompt ("revision", [ "edit" ]);
        description = "Edit a particular revision";
      };
      {
        key = 'd';
        cmd = Prompt ("description", [ "describe"; "-m" ]);
        description = "Describe this revision";
      };
      {
        key = 'R';
        cmd = Cmd_I [ "resolve" ];
        description = "Resolve conflicts at this revision";
      };
      {
        key = 'r';
        description = "Rebase revision ";
        cmd =
          SubCmd
            [
              {
                key = 'r';
                description = "Rebase single revision";
                cmd =
                  Prompt ("destination for revision rebase", [ "rebase"; "-r"; "@"; "-d" ]);
              };
              {
                key = 's';
                description = "Rebase revision and its decendents";
                cmd =
                  Prompt
                    ("Destination for decendent rebase", [ "rebase"; "-s"; "@"; "-d" ]);
              };
              {
                key = 'b';
                description = "Rebase revision and all other revissions on its branch";
                cmd =
                  Prompt ("Destination for branch rebase", [ "rebase"; "-b"; "@"; "-d" ]);
              };
            ];
      };
      {
        key = 'g';
        description = "Git commands";
        cmd =
          SubCmd
            [
              { key = 'p'; description = "git push branch"; cmd = Cmd [ "git"; "push" ] };
              { key = 'f'; description = "git fetch"; cmd = Cmd [ "git"; "fetch" ] };
            ];
      };
      {
        key = 'z';
        description =
          "Parallelize commits. Takes 2 commits and makes them have the\n\
           same parent and child. Run `jj parallelize` --help for details";
        cmd =
          PromptThen
            ( "list commits to parallelize",
              fun x -> Cmd ([ "paralellize" ] @ (x |> String.split_on_char ' ')) );
      };
      {
        key = 'a';
        description = "Abandon this change(removes just this change and rebases parents)";
        cmd =
          SubCmd
            [
              {
                key = 'a';
                description = "Yes i want to abandon the change";
                cmd = Cmd [ "abandon" ];
              };
            ];
      };
      {
        key = 'b';
        description = "Branch commands";
        cmd =
          SubCmd
            [
              {
                key = 'c';
                description = "Create new branches";
                cmd =
                  PromptThen
                    ( "Branch names to create",
                      fun x ->
                        Cmd ([ "branch"; "create" ] @ (x |> String.split_on_char ' ')) );
              };
              {
                key = 'd';
                description = "Delete branches";
                cmd = Prompt ("Branch names to delete", [ "branch"; "delete" ]);
              };
              {
                key = 'r';
                description = "Rename branch";
                cmd =
                  PromptThen
                    ( "Branch to rename",
                      fun curr_name ->
                        Prompt ("New branch name", [ "branch"; "rename"; curr_name ]) );
              };
              {
                key = 's';
                description = "set branch to this change";
                cmd = Prompt ("Branch to set to this commit ", [ "branch"; "set"; "-B" ]);
              };
              {
                key = 't';
                description = "track given remote branch";
                cmd = Prompt ("Branch to track 'branch@remote'", [ "branch"; "track" ]);
              };
              {
                key = 'u';
                description = "untrack given remote branch";
                cmd = Prompt ("Branch to untrack 'branch@remote'", [ "branch"; "untrack" ]);
              };
            ];
      };
    ]
  ;;

  let rec handleCommand description cmd =
    let noOut args =
      let _ = jj args in
      Global_funcs.on_change ();
      ()
    in
    let prompt str cmd =
      ui_state.show_prompt
      $= Some
           ( str,
             "",
             function
             | `Finished str ->
               (match cmd with
                | `Cmd args ->
                  let _result = jj (args @ [ str ]) in
                  Global_funcs.on_change ();
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
    | Prompt_Fn (str, fn) ->
      ui_state.show_popup $= None;
      prompt str (`Cmd (fn()));
      raise Handled
    | Fun func ->
      ui_state.show_popup $= None;
      func ();
      raise Handled
    | SubCmd sub_map ->
      ui_state.show_popup $= Some (commands_list_ui sub_map, description);
      ui_state.input $= `Mode (command_input ~is_sub:true sub_map);
      raise Handled

  (** Try mapching the command mapping to the provided key and run the command if it matches*)
  and command_input ?(is_sub = false) keymap key =
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
