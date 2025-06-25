open Notty
open Nottui
open Lwd_infix
open Jj_tui
open! Util

(** Collection of JJ specific widgets*)

module Make (Vars : Global_vars.Vars) = struct
  open Vars
  open Global_vars
  open Jj_process.Make (Vars)

  (*We use this sepcial char as the seperator because it seems very unlikely anyone will ever use it in a branch name:
    \u{1c}
  *)

  (*List of branches, containing the full output string as well as the string name*)
  let get_branches template =
    let log = jj_no_log ~snapshot:false [ "bookmark"; "list"; "-a"; "-T"; template ] in
    let lines = String.split_on_char '\n' log in
    lines
    |> List.filter_map (fun line ->
      if line |> String.length == 0
      then None
      else
        line
        |> Base.String.lsplit2 ~on:'\x1c'
        |> Option.map (fun (name, rest) -> name, rest))
  ;;

  let get_branches_selectable template () =
    get_branches template
    |> List.map (fun (name, str) ->
      W.Lists.
        {
          data = name
        ; id = name |> String.hash
        ; ui =
            str ^ "\n"
            |> Jj_tui.AnsiReverse.colored_string
            |> Ui.atom
            |> Ui.resize ~w:100 ~h:1 ~mw:100
            |> W.Lists.selectable_item
        })
  ;;

  let branches_no_remote =
    get_branches_selectable
      ({|if(!remote,name++"|}
       ^ "\u{1C}"
       ^ {|"++label("bookmark", name)  ++ if(present, format_ref_targets(self), " (deleted)")++ "\n")|}
      )
  ;;

  let branches_remotes_no_git =
    get_branches_selectable
    @@ {|if(remote && !(remote.starts_with("git")&&remote.ends_with("git")), name++"|}
    ^ "\u{1C}"
    ^ {|"++label("bookmark", name++" @"++remote)  ++ if(present, format_ref_targets(self), " (deleted)")++ "\n")|}
  ;;

  let branches_remotes_not_tracked =
    get_branches_selectable
    @@ {|if(remote && !(remote.starts_with("git")&&remote.ends_with("git")) && !tracked, name++"@"++remote++"|}
    ^ "\u{1C}"
    ^ {|"++label("bookmark", name++" @"++remote)  ++ if(present, format_ref_targets(self), " (deleted)")++ "\n")|}
  ;;

  let branches_remotes_tracked =
    get_branches_selectable
    @@ {|if(remote && !(remote.starts_with("git")&&remote.ends_with("git")) && tracked, name++"@"++remote++"|}
    ^ "\u{1C}"
    ^ {|"++label("bookmark", name++" @"++remote)  ++ if(present, format_ref_targets(self), " (deleted)")++ "\n")|}
  ;;

  let get_remotes () =
    let log = jj_no_log ~snapshot:false [ "git"; "remote"; "list" ] in
    let lines = String.split_on_char '\n' log in
    lines
    |> List.filter_map (fun line ->
      if line |>String.trim|> String.length =0
      then None
      else (
        match Base.String.lsplit2 ~on:' ' line with
        | Some (name, _) -> Some (name, line)
        | None -> Some (line, line)))
  ;;

  let get_remotes_selectable () =
    get_remotes ()
    |> List.map (fun (name, str) ->
      W.Lists.
        {
          data = name
        ; id = name |> String.hash
        ; ui =
            str ^ "\n"
            |> Jj_tui.AnsiReverse.colored_string
            |> Ui.atom
            |> Ui.resize ~w:100 ~h:1 ~mw:100
            |> W.Lists.selectable_item
        })
  ;;

  let selection_list ?(focus = Focus.make ()) items =
    Focus.request focus;
    let selected_var = Lwd.var 0 in
    let items =
      let$ selected = Lwd.get selected_var
      and$ focus = focus |> Focus.status
      and$ items = items in
      items
      |> List.mapi (fun i x -> if selected == i then x true else x false)
      |> Ui.vcat
      |> Ui.keyboard_area ~focus (function
        | `Arrow `Up, [] ->
          selected_var $= max (Lwd.peek selected_var - 1) 0;
          `Handled
        | `Arrow `Down, [] ->
          selected_var $= min (Lwd.peek selected_var + 1) ((items |> List.length) - 1);
          `Handled
        | _ ->
          `Unhandled)
    in
    items
  ;;

  (**A weird function to replace the start of a string if it matches the prefix. Only replacing at most as much as the matched prefix *)
  let unsafe_blit_start_if prefix replace string =
    if String.starts_with string ~prefix
    then (
      (*TODO could do this unsafe for more speed*)
      let padding = (replace |> String.length) - (prefix |> String.length) in
      let string = String.make padding ' ' ^ string in
      Bytes.blit_string
        replace
        0
        (*we pad it to make sure we aren't going to blit over more than the prefix*)
        (Bytes.unsafe_of_string string)
        0
        (replace |> String.length);
      string)
    else string
  ;;

  (* (*TODO:make a custom widget the renders the commit with and without selection. *)
     (* with selection replace the dot with a blue version and slightly blue tint the background *) *)
  (* let revs_list_filtered () = *)
  (* let graph_items, revs = seperate_revs () in *)
  (* graph_items *)
  (* |> List.map (fun x is_focused -> *)
  (* hightlight blue when selection is true *)
  (* let prefix = *)
  (* if is_focused then I.char A.(bg A.blue) '>' 1 2 else I.char A.empty ' ' 1 2 *)
  (* in *)
  (* I.hcat *)
  (* [ *)
  (* prefix *)
  (* ; x ^ "\n" |> unsafe_blit_start_if "@" "◉" |> Jj_tui.AnsiReverse.colored_string *)
  (* ] *)
  (* |> Ui.atom) *)
  (* |> List.map2 (fun rev ui -> Wd.{ ui; data = rev }) revs *)
  (* |> Lwd.pure *)
  (* |> Wd.filterable_selection_list *)
  (* ~on_confirm:(fun _ -> ()) *)
  (* ~filter_predicate:(fun input rev -> rev |> String.starts_with ~prefix:input) *)
  (* ;; *)

  (** Start a process that will take full control of both stdin and stdout.
      This is used for interactive diffs and such*)
  let interactive_process cmd =
    let post_change new_view =
      Global_funcs.update_status ();
      Lwd.set ui_state.view new_view
    in
    let exit_status_to_str (y : Unix.process_status) stderr_string =
      let open Unix in
      match match y with WSTOPPED x -> x | WEXITED x -> x | WSIGNALED x -> x with
      | 0 ->
        None
      | 1 ->
        Some (Printf.sprintf "1, Failure\nStderr:\n%s" stderr_string)
      | a ->
        Some (Printf.sprintf "unknown code %d\nStderr:\n%s" a stderr_string)
    in
    let exit_code, stderr_string = switch_to_process (cmd |> Array.of_list) in
    let error_msg = exit_status_to_str exit_code stderr_string in
    match error_msg with
    | None ->
      post_change `Main;
      Ui.empty |> Lwd.pure
    | Some exit_msg ->
      let$ ui =
        W.vbox
          [
            W.string (Printf.sprintf "exit code: %s" exit_msg) |> Lwd.pure
          ; W.button "Press space to return to main UI" (fun _ -> post_change `Main)
            |> Lwd.pure
          ]
      in
      ui
      |> Ui.keyboard_area (fun event ->
        match event with
        | `Escape, _ | `Enter, _ | `ASCII ' ', _ ->
          post_change `Main;
          `Handled
        | _ ->
          `Unhandled)
  ;;
end
