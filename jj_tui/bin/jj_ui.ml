open Notty
open Nottui
open Lwd_infix
module W = Nottui_widgets

module Ui = struct
  include Nottui.Ui

  (* let pad ?(l=0) ?(r=0) ?(t=0) ?(b=0) ui= *)
  (* match l,r,t,b with *)
  (* |(0,0,_,_)-> *)
  (* |(_,_,0,0)-> *)
  (* |(_,_,_,_)-> *)
  let pad v h ui =
    let o_w = Ui.layout_width ui in
    let o_h = Ui.layout_height ui in
    Ui.resize ~w:(o_w + h) ~h:(o_h + v) ui
  ;;
end

module Make (Vars : Global_vars.Vars) = struct
  open Jj_process.Make (Vars)
  open Vars
  module Jj_commands = Jj_commands.Make (Vars)

  (* let task_pool () = *)
  (* Option.get !Vars.pool *)

  let full_term_sized_background =
    let$ term_width, term_height = Lwd.get Vars.term_width_height in
    Notty.I.void term_width term_height |> Nottui.Ui.atom
  ;;

  let colored_string = Jj_tui.AnsiReverse.colored_string

  (* let vQuit = Lwd.var false *)

  let _quitButton =
    W.button (Printf.sprintf "quit ") (fun () -> Vars.quit $= true) |> Lwd.pure
  ;;

  let ( <-$ ) f v = Lwd.map ~f (Lwd.get v)
  let ( $-> ) v f = Lwd.map ~f (Lwd.get v)
  let ( |>$ ) v f = Lwd.map ~f v
  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)

  (* let ( let<- ) v f = Lwd.map ~f (Lwd.get v) *)

  let on_change () =
    let res = jj_no_log [ "show" ] |> colored_string in
    ui_state.jj_show $= res;
    let res = jj_no_log [] |> colored_string in
    ui_state.jj_tree $= res
  ;;

  let post_change new_view =
    on_change ();
    Lwd.set ui_state.view new_view
  ;;

  let inputs ui =
    let$ input_state = Lwd.get ui_state.input in
    let handler =
      match input_state with
      | `Normal ->
        Jj_commands.handle_inputs
      | `Mode handle ->
        handle
    in
    ui
    |> Ui.event_filter @@ fun event ->
       match event with
       | `Key (`ASCII 'q', _) ->
         Vars.quit $= true;
         `Handled
       | `Key (`ASCII key, _) ->
         (match handler key with
          | `Handled ->
            on_change ();
            `Handled
          | `Unhandled ->
            `Unhandled)
       | _ ->
         `Unhandled
  ;;

  (** Start a process that will take full control of both stdin and stdout.
      This is used for interactive diffs and such*)
  let interactive_process env cmd =
    let exit_status_to_str y =
      match match y with `Exited x -> x | `Signaled x -> x with
      | 0 ->
        "success"
      | 1 ->
        "failure%s"
      | a ->
        Printf.sprintf "unknown code %d" a
    in
    let res = switch_to_process env cmd in
    let$ ui =
      W.vbox
        [
          W.string (Printf.sprintf "exit code:%s" (res |> exit_status_to_str)) |> Lwd.pure;
          W.button "back to main UI" (fun _ -> post_change `Main) |> Lwd.pure;
        ]
    in
    ui
    |> Ui.event_filter (fun event ->
      match event with
      | `Key (`ASCII ' ', _) ->
        post_change `Main;
        `Handled
      | _ ->
        `Unhandled)
  ;;

  (* let squashButton = *)
  (* W.button "squash" (fun _ -> Lwd.set ui_state.view (`Cmd [ "jj"; "squash"; "-i" ])) *)
  (* ;; *)

  let mainUi env =
    (*we want to initialize our states*)
    on_change ();
    let$* running = Lwd.get ui_state.view in
    match running with
    | `Cmd cmd ->
      (*We have this extra step to paint the terminal empty for one step*)
      Lwd.set ui_state.view @@ `RunCmd cmd;
      full_term_sized_background
    | `RunCmd cmd ->
      interactive_process env cmd
    | (`Main | `Prompt _) as rest ->
      let v_cmd_out = Lwd.var "" in
      let scrollState = Lwd.var W.default_scroll_state in
      let pane =
        W.h_pane
          (W.vbox
             [
               ui_state.jj_tree $-> (I.pad ~l:1 ~r:1 >> Ui.atom);
               W.string "━━━━━━━━━━━━━━━━━━" |> Lwd.pure;
               ui_state.command_log
               |> Lwd.get
               |> Lwd.bind ~f:(List.map (W.string >> Lwd.pure) >> W.vlist);
               v_cmd_out $-> W.string;
             ])
          (W.vscroll_area
             ~change:(fun _action state -> scrollState $= state)
             ~state:(Lwd.get scrollState)
             ((fun x -> x |> I.pad ~l:1 ~r:1 |> Ui.atom) <-$ ui_state.jj_show))
      in
      (match rest with
       | `Main ->
         pane |> Lwd.bind ~f:inputs
       | `Prompt (name, cmd) ->
         W.zbox
           [
             pane;
             Widgets.prompt
               (function
                 | `Finished str ->
                   v_cmd_out $= jj (cmd @ [ str ]);
                   post_change `Main
                 | `Closed ->
                   post_change `Main)
               name
             |>$ Ui.resize ~pad:Widgets.neutral_grav;
           ])
  ;;
end
