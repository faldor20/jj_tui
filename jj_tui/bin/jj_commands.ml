module Make (Vars : Global_vars.Vars) = struct
  open Vars
  open Jj_process.Make (Vars)

  let handle_inputs key =
    let noOut args =
      let _ = jj args in
      `Handled
    in
    let change_view view =
      Lwd.set ui_state.view view;
      `Handled
    in
    let change_view_sub view =
      Lwd.set ui_state.view view;
      Lwd.set ui_state.input `Normal;
      `Handled
    in
    let send_cmd args = change_view (`Cmd args) in
    let send_cmd_sub args = change_view_sub (`Cmd args) in
    match key with
    | 'P' ->
      noOut [ "prev" ]
    | 'p' ->
      noOut [ "prev"; "--edit" ]
    | 'N' ->
      noOut [ "next" ]
    | 'n' ->
      noOut [ "next"; "--edit" ]
    | 'h' ->
      noOut [ "new" ]
    | 'c' ->
      change_view @@ `Prompt ("commit msg", [ "commit"; "-m" ])
    | 'S' ->
      send_cmd [ "unsquash"; "-i" ]
    | 's' ->
      `Mode
        (function
          | 's' ->
            send_cmd_sub [ "squash" ]
          | 'i' ->
            send_cmd_sub [ "squash"; "-i" ]
          | _ ->
            `Unhandled)
      |> Lwd.set ui_state.input;
      `Handled
    | 'm' ->
      `Mode
        (function
          | 'm' ->
            change_view_sub
            @@ `Prompt ("Move revesion content to:", [ "move"; "-f"; "@"; "-t" ])
          | 'i' ->
            change_view_sub
            @@ `Prompt ("Move revesion content to:", [ "move"; "-i"; "-f"; "@"; "-t" ])
          | _ ->
            `Unhandled)
      |> Lwd.set ui_state.input;
      `Handled
    | 'e' ->
      change_view @@ `Prompt ("revision", [ "edit" ])
    | 'd' ->
      change_view @@ `Prompt ("description", [ "describe"; "-m" ])
    | 'R' ->
      send_cmd [ "resolve" ]
    | 'r' ->
      (* We can move to a different command mode using this mode setup*)
      `Mode
        (function
          | 'm' ->
            change_view_sub
            @@ `Prompt ("destination for revision rebase", [ "rebase"; "-r"; "@"; "-d" ])
          | 'b' ->
            change_view_sub
            @@ `Prompt ("destination for branch rebase", [ "rebase"; "-b"; "@"; "-d" ])
          | _ ->
            `Unhandled)
      |> Lwd.set ui_state.input;
      `Handled
    | _ ->
      `Unhandled
  ;;
end
