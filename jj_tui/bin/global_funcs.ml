open Global_vars
open Lwd_infix
open Jj_process.Make (Global_vars.Vars)

let colored_string = Jj_tui.AnsiReverse.colored_string

(**lists the files in a specific revision*)
let list_files ?(rev = "@") () =
  jj_no_log ~snapshot:false ~color:false [ "diff"; "-r"; rev; "--summary" ]
  |> String.split_on_char '\n'
  |> List.filter_map (fun x ->
    if x |> String.trim <> ""
    then (
      match String.split_on_char ' ' x with
      | [ a; b ] ->
        Some (a, b)
      | _ ->
        failwith
          "Getting files should always return a list of file names with a modifier at \
           the start ")
    else None)
;;

(**Updates the state; Without snapshotting the working copy by default *)
let on_change ?(cause_snapshot = false) () =
  Eio.Switch.run @@ fun sw ->
  Eio.Fiber.fork ~sw (fun _ ->
    let res =
      jj_no_log ~snapshot:cause_snapshot [ "show"; "-s"; "--color-words" ] |> colored_string
    in
    Vars.ui_state.jj_show $= res);
  (*From now on we use ignore-working-copy so we don't re-snapshot the state*)
  Eio.Fiber.fork ~sw (fun _ ->
    let res = jj_no_log ~snapshot:false [ "log" ] |> colored_string in
    Vars.ui_state.jj_tree $= res);
  (* TODO: stop using dop last twice *)
  Eio.Fiber.fork ~sw (fun _ ->
    let res = jj_no_log ~snapshot:false [ "branch"; "list"; "-a" ] |> colored_string in
    Vars.ui_state.jj_branches $= res);
  Eio.Fiber.fork ~sw (fun _ -> Vars.ui_state.jj_change_files $= list_files ())
;;
