module Make (Vars : Global_vars.Vars) = struct
  open Lwd_infix
  open Vars
  open Jj_process.Make (Vars)
  open Notty
  module W = Nottui_widgets
  open Nottui

  type cmd_args = string list

  type command_variant =
    | Cmd of cmd_args
    | Cmd_I of cmd_args
    | Prompt of string * cmd_args

    | PromptThen of string *(string-> ( command_variant))
    | Prompt_I of string * cmd_args
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
        [ I.string A.empty indent; I.char (A.fg A.lightblue) key 1 1; I.strf " %s" desc ]
    in
    commands
    |> List.concat_map @@ fun command ->
         match command with
         | { key; description; cmd = Cmd _ | Cmd_I _ | Prompt _ | Prompt_I _ | Fun _| PromptThen _ } ->
           [ line key description ]
         | { key; description; cmd = SubCmd subs} ->
           line key description :: render_commands ~sub_level:(sub_level + 1) subs
  ;;

  let commands_list_ui commands = commands |> render_commands |> I.vcat |> Ui.atom

  let rec commandMapping =
    [
      {
        key = 'h';
        description = "Show help";
        cmd =
          Fun
            (fun _ ->
              ui_state.show_popup $= Some (commands_list_ui commandMapping, " Help ");
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
                      ui_state.show_popup
                      $= Some (commands_list_ui commandMapping, " Help ");
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
        cmd = Cmd_I ( [ "split";  "-i"]);
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
                      let new_msg =  prev_msg ^ curr_msg in
                      jj [ "squash"; "--quiet"; "-m"; new_msg ] |> ignore);
              };
              {
                key = 'i';
                description = "Interactively choose what to squash into parent";
                cmd = Cmd_I [ "squash"; "-i" ];
              };
            ];
      };
      {
        key = 'm';
        description = "Move changes to another revision";
        cmd =
          SubCmd
            [
              {
                key = 'm';
                description = "Move all changes to another revision";
                cmd = Prompt ("Move revesion content to:", [ "move"; "-f"; "@"; "-t" ]);
              };
              {
                key = 'i';
                description = "Interacitvely select changes to move to another revision";
                cmd =
                  Prompt_I ("Move revision content to:", [ "move"; "-i"; "-f"; "@"; "-t" ]);
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
                    ("destination for decendent rebase", [ "rebase"; "-s"; "@"; "-d" ]);
              };
              {
                key = 'b';
                description = "Rebase revision and all other revissions on its branch";
                cmd =
                  Prompt ("destination for branch rebase", [ "rebase"; "-b"; "@"; "-d" ]);
              };
            ];
      };
      {
        key = 'b';
        description = "Branch commands";
        cmd =           SubCmd
            [
              {
                key = 'c';
                description = "Create new branches";
                cmd =
                  Prompt ("Branch names to create", [ "branch"; "create";  ]);
              };
              {
                key = 'd';
                description = "Delete branches";
                cmd =
                  Prompt
                    ("Branch names to delete", [  "branch"; "delete";   ]);
              };
              {
                key = 'r';
                description = "Rename branch";
                cmd =
                  PromptThen
                    ("Branch to rename",
                     (fun curr_name-> Prompt ("New branch name", ["branch";"rename";curr_name] )));
              };
            ];

      };
    ]
  ;;

  let rec handleCommand description cmd=
    let noOut args =
      let _ = jj args in
      ()
    in
    let prompt str cmd =
      ui_state.show_prompt
      $= Some
           ( Printf.sprintf " %s " str,
             "",
             function
             | `Finished str ->
               (match cmd with
                | `Cmd args ->
                  let _result = jj (args @ [ str ]) in
                  Global_funcs.on_change ();
                  ()
                  (* v_cmd_out $= jj (args @ [ str ]); *)
                | `Cmd_I _ as cmd ->
                  Lwd.set ui_state.view cmd
                |`Fun (func )->
                  func str;
                  
                )
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
          | PromptThen (label,next) ->
            ui_state.show_popup $= None;
            (*We run a prompt that then runs our next command when finished*)
            prompt label @@`Fun (fun x-> next x|>command_no_input description);
            raise Handled
          | Prompt_I (str, args) ->
            ui_state.show_popup $= None;
            prompt str (`Cmd_I args);
            raise Handled
          | Fun func ->
            ui_state.show_popup $= None;
            func ();
            raise Handled
          | SubCmd sub_map ->
            ui_state.show_popup
            $= Some (commands_list_ui sub_map, Printf.sprintf " %s " description);
            ui_state.input $= `Mode (command_input ~is_sub:true sub_map);
            raise Handled


  and  command_input ?(is_sub = false) keymap key =
    (* Use exceptions so we can break out of the list*)
    try
      keymap
      |> List.iter (fun cmd ->
        if cmd.key == key
        then (
        handleCommand cmd.description cmd.cmd
        )
        else ());
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

