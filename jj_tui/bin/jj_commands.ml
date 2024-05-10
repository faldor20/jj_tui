module Make (Vars : Global_vars.Vars) = struct
  open Vars
  open Jj_process.Make (Vars)

  type cmd_args = string list

  type command_variant =
    | Cmd of cmd_args
    | Cmd_I of cmd_args
    | Prompt of string * cmd_args
    | Prompt_I of string * cmd_args
    | SubCmd of command list

  and command = {
    key : char;
    cmd : command_variant;
  }

  type command_list = command list

  exception Handled

  let commandMapping =
    [
      { cmd = Cmd [ "prev" ]; key = 'P' };
      { cmd = Cmd [ "prev"; "--edit" ]; key = 'p' };
      { cmd = Cmd [ "next" ]; key = 'N' };
      { cmd = Cmd [ "next"; "--edit" ]; key = 'n' };
      { cmd = Cmd [ "new" ]; key = 'h' };
      { cmd = Prompt ("commit msg", [ "commit"; "-m" ]); key = 'c' };
      { cmd = Cmd_I [ "unsquash"; "-i" ]; key = 'S' };
      {
        key = 's';
        cmd =
          SubCmd
            [
              { key = 's'; cmd = Cmd [ "squash" ] };
              { key = 'i'; cmd = Cmd_I [ "squash"; "-i" ] };
            ];
      };
      {
        key = 'm';
        cmd =
          SubCmd
            [
              {
                key = 'm';
                cmd = Prompt ("Move revesion content to:", [ "move"; "-f"; "@"; "-t" ]);
              };
              {
                key = 'i';
                cmd =
                  Prompt_I ("Move revesion content to:", [ "move"; "-i"; "-f"; "@"; "-t" ]);
              };
            ];
      };
      { cmd = Prompt ("revision", [ "edit" ]); key = 'e' };
      { cmd = Prompt ("description", [ "describe"; "-m" ]); key = 'd' };
      { cmd = Cmd_I [ "resolve" ]; key = 'R' };
      {
        key = 'r';
        cmd =
          SubCmd
            [
              {
                key = 'm';
                cmd =
                  Prompt ("destination for revision rebase", [ "rebase"; "-r"; "@"; "-d" ]);
              };
              {
                key = 'b';
                cmd =
                  Prompt ("destination for branch rebase", [ "rebase"; "-b"; "@"; "-d" ]);
              };
            ];
      };
    ]
  ;;

  let rec command_input ?(is_sub = false) keymap key =
    let noOut args =
      let _ = jj args in
      ()
    in
    let change_view view = Lwd.set ui_state.view view in
    let send_cmd args = change_view (`Cmd_I args) in
    (* Use exceptions so we can break out of the list*)
    try
      keymap
      |> List.iter (fun cmd ->
        if cmd.key == key
        then (
          match cmd.cmd with
          | Cmd_I args ->
            send_cmd args;
            raise Handled
          | Cmd args ->
            noOut args;
            raise Handled
          | Prompt (str, args) ->
            change_view (`Prompt (str, `Cmd args));
            raise Handled
          | Prompt_I (str, args) ->
            change_view (`Prompt (str, `Cmd_I args));
            raise Handled
          | SubCmd sub_map ->
            `Mode (command_input ~is_sub:true sub_map) |> Lwd.set ui_state.input)
        else ());
      `Unhandled
    with
    | Handled ->
      if is_sub then Lwd.set ui_state.input `Normal;
      `Handled
  ;;
end
