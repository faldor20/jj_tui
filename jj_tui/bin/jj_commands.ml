module Make (Vars : Global_vars.Vars) = struct
  open Vars
  open Jj_process.Make (Vars)

  let handle_inputs key =
    let noOut args =
      let _ = jj args in
      `Handled
    in
    let change_view view = Lwd.set ui_state.view view in
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
      change_view @@ `Prompt ("commit msg", [ "commit"; "-m" ]);
      `Handled
    | 'S' ->
      change_view @@ `Cmd [ "jj"; "unsquash"; "-i" ];
      `Handled
    | 's' ->
      change_view @@ `Cmd [ "jj"; "squash"; "-i" ];
      `Handled
    | 'R' ->
      change_view @@ `Cmd [ "jj"; "resolve" ];
      `Handled
    | 'e' ->
      change_view @@ `Prompt ("revision", [ "edit" ]);
      `Handled
    | 'd' ->
      change_view @@ `Prompt ("description", [ "describe"; "-m" ]);
      `Handled
    | 'm' ->
      change_view @@ `Prompt ("destination", [ "rebase"; "-r"; "@"; "-d" ]);
      `Handled
    | _ ->
      `Unhandled
  ;;
end
