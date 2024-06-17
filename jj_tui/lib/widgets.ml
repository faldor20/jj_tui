open Notty
open Nottui
open Lwd_infix
open! Util
open! Widgets_citty
module W = Nottui_widgets

let dynamic_width = dynamic_width

let dynamic_size ?(w = 10) ~sw ?(h = 10) ~sh f =
  let size = Lwd.var (w, h) in
  let body = f (Lwd.get size) in
  body
  |> Lwd.map ~f:(fun ui ->
    ui
    |> Ui.resize ~w ~sw ~h ~sh
    |> Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then Lwd.set size (w, h)))
;;

let size_logger ui =
  let size = Lwd.var (-1, -1) in
  W.vbox
    [
      (size |> Lwd.get |>$ fun (w, h) -> W.fmt "w:%d,h:%d" w h)
    ; ui
      |>$ Ui.size_sensor (fun ~w ~h ->
        if Lwd.peek size <> (w, h) then Lwd.set size (w, h))
    ]
;;

let neutral_grav = Gravity.make ~h:`Neutral ~v:`Neutral
let make_even num = num + (num mod 2 * 1)
let upPipe = Uchar.of_int 0x2503
let tlPipe = Uchar.of_int 0x250f
let trPipe = Uchar.of_int 0x2513
let blPipe = Uchar.of_int 0x2517
let brPipe = Uchar.of_int 0x251b
let sidePipe = Uchar.of_int 0x2501

(** makes a line of chars with a specific start , midlle and end*)
let make_with_ends start mid ending width =
  Array.init width (fun i ->
    let lastIdx = width - 1 in
    match i with 0 -> start | a when a == lastIdx -> ending | _ -> mid)
  |> I.uchars A.empty
;;

let make_top width = make_with_ends tlPipe sidePipe trPipe width
let make_bot width = make_with_ends blPipe sidePipe brPipe width

(** This is for shifting something away from the edge it is pushed against *)
let pad_edge x_pad y_pad grav ui =
  let y_pad =
    match grav |> Gravity.v with
    | `Negative ->
      -y_pad
    | `Neutral ->
      0
    | `Positive ->
      y_pad
  in
  match grav |> Gravity.h with
  | `Negative ->
    ui |> Ui.shift_area (-x_pad) y_pad
  | `Neutral ->
    ui
  | `Positive ->
    ui |> Ui.shift_area x_pad y_pad
;;

let grid xxs = xxs |> List.map I.hcat |> I.vcat

(** Truncate a string to a given length, adding an ellipsis if truncated. *)
let truncate_string len str =
  if String.length str > len
  then if len <= 3 then "" else String.sub str 0 (len - 3) ^ "..."
  else str
;;

(** top border*)
let outline_top attr w label =
  let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
  let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
  and label = if label |> I.width > w - 2 then I.empty else label |> I.hpad 2 0
  and a, b = chr 0x256d, chr 0x256e in
  I.zcat [ label; I.hcat [ a; hbar; b ]; label ]
;;

(** bottom border*)
let outline_bot attr w label =
  let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
  let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1 in
  let label =
    if label |> I.width > w - 2
    then I.empty
    else label |> I.hpad (w - (label |> I.width |> ( + ) 1)) 0
  in
  let c, d = chr 0x256f, chr 0x2570 in
  I.zcat [ label; I.hcat [ d; hbar; c ] ]
;;

let make_label max_width label_str =
  I.strf " %s " (truncate_string (max_width - 2) label_str)
;;

(** Internal function for rendering a border box with known dimensions and padding.*)
let border_box_intern
  ?(border_attr = A.empty)
  ?(label_top = I.empty)
  ?(label_bottom = I.empty)
  w
  h
  pad
  pad_w
  pad_h
  input
  =
  (* this is a weird quirk, but we have to be careful of runaway size expansion.
     If we increase the width of the space by making the vbar longer than the input ui element it will be able to expand to fill that space.
     That will then increase the vbar and increase the height etc etc untill the max height is reached*)
  let vbar =
    I.uchar border_attr (Uchar.of_int 0x2502) 1 (h + (pad_h * 2))
    |> Ui.atom
    |> Ui.resize ~h:0
  in
  Ui.vcat
    [
      outline_top border_attr w label_top |> Ui.atom |> Ui.resize ~w:0
    ; Ui.hcat
        [
          vbar
        ; I.void pad_w 1 |> Ui.atom
        ; Ui.vcat
            [
              I.void 1 pad_h |> Ui.atom
            ; input |> Ui.resize ~pad
            ; I.void 1 pad_h |> Ui.atom
            ]
        ; I.void pad_w 1 |> Ui.atom
        ; vbar
        ]
    ; outline_bot border_attr w label_bottom |> Ui.atom |> Ui.resize ~w:0
    ]
;;

let border_box_custom_border
  ?(scaling = `Static)
  ?(pad = neutral_grav)
  ?(pad_w = 2)
  ?(pad_h = 1)
  ?label_top
  ?label_bottom
  get_border
  input
  =
  let max_width, min_width, sw =
    match scaling with
    | `Static ->
      None, None, None
    (* This allows the input to expand to fill space*)
    | `Expand sw ->
      Some 1000, None, Some sw
  in
  let size = Lwd.var (0, 0) in
  let layout_width = Lwd.var 0 in
  let input =
    let$ input = input |>$ Ui.resize ?sw ?mw:max_width ?sh:sw ?mh:max_width in
    (*We need this later to determine the max with*)
    layout_width $= (input |> Ui.layout_width);
    input
    (*This lets us tell the input to be a flexible size*)
    |> Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then Lwd.set size (w, h))
  in
  (*This is original width and height of the input before padding or anything *)
  let$ o_w, o_h = Lwd.get size
  and$ input = input
  and$ border_attr = get_border in
  let w, h = o_w + (pad_w * 2), o_h in
  let h = h in
  let bbox =
    border_box_intern
      ~border_attr
      ?label_top:(label_top |> Option.map (make_label (w - 2)))
      ?label_bottom:(label_bottom |> Option.map (make_label (w - 2)))
      (* ~label_bottom:(if has_focus then I.strf "focused" else I.strf "unfocused") *)
      w
      h
      pad
      pad_w
      pad_h
      input
  in
  (*If we want the input to be shrinkable we make it expandable, set it's width to something small and then set a max width for the whole box*)
  bbox
;;

(* FIXME I have no clue why, but without this second resize the max width stuff doesn't work *)
(* |> Ui.resize *)

(** Creates a bordered box around the given [input] widget. This box will change colour when focused

    @param scaling
      Controls how the input widget is sized within the border box. Can be:
      - [`Static] - The input widget is not resized.
      - [`Expand sw] - The input widget is allowed to expand to fill the available space, with a stretch width [sw].
      - [`Shrinkable (min_width, sw)] - The input widget is allowed to shrink to a minimum width of [min_width], and expand with a stretch width [sw].
    @param pad The padding around the input widget within the border box.
    @param pad_w The horizontal padding around the input widget.
    @param pad_h The vertical padding around the input widget.
    @param label An optional label to display within the border box.
    @param input The input widget to be bordered.
    @param border_attr Style for the border, defaults to [A.empty].
    @param focus Focus handle for the box .
    @param focus_attr Style for the border when focused, defaults to [A.fg A.blue]. *)
let border_box_focusable
  ?scaling
  ?pad
  ?pad_w
  ?pad_h
  ?label_top
  ?label_bottom
  ?(border_attr = A.empty)
  ?(focus_attr = A.fg A.blue)
  ?(focus = Focus.make ())
  input
  =
  let attr = Lwd.var border_attr in
  let input =
    input
    |> Lwd.map2 (focus |> Focus.status) ~f:(fun focus ui ->
      ui |> Ui.keyboard_area ~focus (fun _ -> `Unhandled))
  in
  border_box_custom_border
    ?scaling
    ?pad
    ?pad_w
    ?pad_h
    ?label_top
    ?label_bottom
    (let$ focus = Focus.status focus in
     if Focus.has_focus focus then focus_attr else border_attr)
    input
;;

(** Creates a bordered box around the given [input] widget.

    @param scaling
      Controls how the input widget is sized within the border box. Can be:
      - [`Static] - The input widget is not resized.
      - [`Expand sw] - The input widget is allowed to expand to fill the available space, with a stretch width [sw].
      - [`Shrinkable (min_width, sw)] - The input widget is allowed to shrink to a minimum width of [min_width], and expand with a stretch width [sw].
    @param pad The padding around the input widget within the border box.
    @param pad_w The horizontal padding around the input widget.
    @param pad_h The vertical padding around the input widget.
    @param label An optional label to display within the border box.
    @param input The input widget to be bordered.
    @param border_attr Style for the border, defaults to [A.empty]. *)
let border_box
  ?scaling
  ?pad
  ?pad_w
  ?pad_h
  ?label_top
  ?label_bottom
  ?(border_attr = A.empty)
  input
  =
  border_box_custom_border
    ?scaling
    ?pad
    ?pad_w
    ?pad_h
    ?label_top
    ?label_bottom
    (border_attr |> Lwd.pure)
    input
;;

(** image outline creator*)
let outline attr w h =
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
  and vbar = I.uchar attr (Uchar.of_int 0x2502) 1 h in
  let a, b, c, d = chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570 in
  let void = I.void w h in
  grid [ [ a; hbar; b ]; [ vbar; void; vbar ]; [ d; hbar; c ] ]
;;

(** image outline creator*)
let label_outline ~label ~label_bottom attr i =
  let w, h = I.(width i, height i) in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and top_label = label |> I.hpad 1 0
  and bot_label = label_bottom |> I.hpad (w - (label_bottom |> I.width |> ( + ) 1)) 0
  and hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
  and vbar = I.uchar attr (Uchar.of_int 0x2502) 1 h in
  (*now we layer the bars with our labels ontop*)
  let hbar_top = [ top_label; hbar ] |> I.zcat
  and hbar_bot = [ bot_label; hbar ] |> I.zcat in
  let tl, tr, br, bl = chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570 in
  grid [ [ tl; hbar_top; tr ]; [ vbar; i; vbar ]; [ bl; hbar_bot; br ] ]
;;

let label_outline_strings ~label ~label_bottom ?(attr = A.empty) i =
  label_outline
    ~label:(I.string attr label)
    ~label_bottom:(I.string attr label_bottom)
    attr
    i
;;

(*
   design for a windowing system:
   fundinmental primative:
   window stack:
   when inside a window stack you can navigate up and down your stack.
   if you would also like to navigate to stacks sideways you just wrap then in a horizontal window stack.
*)
let v_window_stack ~focus windows =
  let focused = ref 0 in
  let is_focused = ref false in
  let windows, focuses =
    windows
    |> List.map (fun window_maker ->
      let focus = Focus.make () in
      window_maker ~focus, focus)
    |> List.split
  in
  focuses |> List.hd |> Focus.request;
  (let$ ui = W.vbox windows
   and$ focus = focus |> Focus.status in
   if (not !is_focused) && Focus.has_focus focus
   then (
     is_focused := true;
     List.nth focuses !focused |> Focus.request)
   else if !is_focused && not (Focus.has_focus focus)
   then is_focused := false;
   ui
   |> Ui.join_x (if !is_focused then W.string "f" else W.string "u")
   |> Ui.keyboard_area
        ~focus
        (if !is_focused
         then
           function
           | `Arrow `Down, [ `Ctrl ] ->
             let focused_idx = !focused in
             (* set the focus to be the next item if possible *)
             List.nth_opt focuses (focused_idx + 1)
             |> Option.iter (fun x ->
               List.nth focuses focused_idx |> Focus.release;
               Focus.request x;
               focused := focused_idx + 1);
             `Handled
           | `Arrow `Up, [ `Ctrl ] ->
             let focused_idx = !focused in
             let target_idx = focused_idx - 1 in
             if target_idx >= 0
             then
               (* set the focus to be the previous item if possible *)
               List.nth_opt focuses target_idx
               |> Option.iter (fun x ->
                 List.nth focuses focused_idx |> Focus.release;
                 Focus.request x;
                 focused := focused_idx - 1);
             `Handled
           | _ ->
             `Unhandled
         else fun x -> `Unhandled))
  |> border_box_custom_border
       (let$ focus = focus |> Focus.status |>$ Focus.has_focus in
        if focus then A.fg A.blue else A.empty)
;;

let h_window_stack ~focus windows =
  let focused = ref 0 in
  let is_focused = ref false in
  let windows, focuses =
    windows
    |> List.map (fun window_maker ->
      let focus = Focus.make () in
      window_maker ~focus, focus)
    |> List.split
  in
  (let$ ui = W.hbox windows
   and$ focus = focus |> Focus.status in
   if (not !is_focused) && Focus.has_focus focus
   then (
     is_focused := true;
     List.nth focuses !focused |> Focus.request)
   else if !is_focused && not (Focus.has_focus focus)
   then is_focused := false;
   ui
   |> Ui.keyboard_area
        ~focus
        (if !is_focused
         then
           function
           | `Arrow `Right, [ `Ctrl ] ->
             let focused_idx = !focused in
             (* set the focus to be the next item if possible *)
             List.nth_opt focuses (focused_idx + 1)
             |> Option.iter (fun x ->
               List.nth focuses focused_idx |> Focus.release;
               Focus.request x;
               focused := focused_idx + 1);
             `Handled
           | `Arrow `Left, [ `Ctrl ] ->
             let focused_idx = !focused in
             let target_idx = focused_idx - 1 in
             if target_idx >= 0
             then
               (* set the focus to be the previous item if possible *)
               List.nth_opt focuses target_idx
               |> Option.iter (fun x ->
                 List.nth focuses focused_idx |> Focus.release;
                 Focus.request x;
                 focused := focused_idx - 1);
             `Handled
           | _ ->
             `Unhandled
         else fun x -> `Unhandled))
  |> border_box_custom_border
       (let$ focus = focus |> Focus.status |>$ Focus.has_focus in
        if focus then A.fg A.blue else A.empty)
;;

(* let v_window_stack ?focus windows = *)
(* let focused = ref 0 in *)
(* let windows, focuses = *)
(* windows *)
(* |> List.map (fun window_maker -> *)
(* let focus = Focus.make () in *)
(* window_maker ~focus, focus) *)
(* |> List.split *)
(* in *)
(* focuses |> List.hd |> Focus.request; *)
(* W.vbox windows *)
(* |>$ Ui.keyboard_area (function *)
(* | `ASCII 'p', [] -> *)
(* `Remap (`Focus `Up, []) *)
(* | `ASCII 'n', [ `Shift ] -> *)
(* `Remap (`Focus `Down, []) *)
(* | _ -> *)
(* `Unhandled) *)
(* ;; *)

let v_window_stack2 windows =
  let focus = Focus.make () in
  let$ focus = focus |> Focus.status
  and$ ui = W.vbox windows in
  ui
  |> Ui.keyboard_area ~focus (function
    | `ASCII 'p', [] ->
      `Handled
    | `ASCII 'n', [] ->
      `Handled
    | `ASCII 'i', [] ->
      `Handled
    | `ASCII 'e', [] ->
      Out_channel.with_open_text "Out" (fun chan -> Out_channel.output_string chan "out");
      `Remap (`Focus `Out, [])
    | _ ->
      `Unhandled)
;;

let h_window_stack2 windows =
  let focus = Focus.make () in
  let$ focus = focus |> Focus.status
  and$ ui = W.hbox windows in
  ui
  |> Ui.keyboard_area ~focus (function
    | `ASCII 'p', [] ->
      `Handled
    | `ASCII 'n', [] ->
      `Handled
    | `ASCII 'e', [] ->
      Out_channel.with_open_text "Out" (fun chan -> Out_channel.output_string chan "out");
      `Remap (`Focus `Out, [])
    | _ ->
      `Unhandled)
;;

(**Tries to fix the issue with the current layout system in which we cannot enter/exit a particular nodes selection tree *)
let v_window_stack2 windows =
  let focus = Focus.make () in
  let$ focus = focus |> Focus.status
  and$ ui = W.vbox windows in
  ui
  |> Ui.keyboard_area ~focus (function
    | `ASCII 'p', [] ->
      `Handled
    | `ASCII 'n', [] ->
      `Handled
    | `ASCII 'i', [] ->
      `Handled
    | `ASCII 'e', [] ->
      Out_channel.with_open_text "Out" (fun chan -> Out_channel.output_string chan "out");
      `Remap (`Focus `Out, [])
    | _ ->
      `Unhandled)
;;

(** ui outline creator*)
let ui_outline
  ?(pad = neutral_grav)
  ?(pad_w = 4)
  ?(pad_h = 2)
  ?(attr = A.empty)
  ?(label = "")
  ?(label_bottom = "")
  ui
  =
  dynamic_size ~sw:1 ~sh:1 @@ fun size ->
  let$ w, h = size in
  let outline =
    label_outline_strings
      ~label_bottom
      ~label
      ~attr
      (I.char A.empty ' ' (w + pad_w) (h + pad_h))
    |> Ui.atom
  in
  Ui.zcat [ outline; ui |> Ui.resize ~sw:0 ~pad |> pad_edge (pad_w / 2) (pad_h / 2) pad ]
;;

let monitor ui =
  ui |> Ui.layout_spec |> Ui.pp_layout_spec Format.str_formatter;
  let str = Format.flush_str_formatter () in
  let log =
    str |> String.split_on_char '\n' |> List.map (I.string A.empty) |> I.vcat |> Ui.atom
  in
  Ui.vcat [ ui; log ]
;;

(**clears anything behind the given area*)
let set_bg attr ui =
  let size = Lwd.var (0, 0) in
  W.zbox
    [
      ( size |> Lwd.get |>$ fun (w, h) ->
        I.char attr ' ' w h |> Ui.atom |> Ui.resize ~w:0 ~h:0 )
    ; ui |>$ Ui.size_sensor (fun ~w ~h -> if (w, h) <> Lwd.peek size then size $= (w, h))
    ]
;;

(**Clears anything behind the given area using the width. If you have a dynamic sized element use [set_bg]*)
let set_bg_static attr ui =
  let w, h = Ui.layout_width ui, Ui.layout_height ui in
  Ui.zcat [ I.char attr ' ' w h |> Ui.atom |> Ui.resize ~w:0 ~h:0; ui ]
;;

(**clears anything behind the given area*)
let clear_bg ui = set_bg A.empty ui

(*========Prompt=======*)
let prompt onExit name =
  let prompt_input = Lwd.var ("", 0) in
  let prompt_val = Lwd.get prompt_input in
  let$* prompt_field =
    W.zbox
      [
        W.string ~attr:A.(st underline) "                                       "
        |> Lwd.pure
      ; W.edit_field
          prompt_val
          ~on_change:(fun state -> Lwd.set prompt_input state)
          ~on_submit:(fun (str, _) -> onExit (`Finished str))
      ]
  in
  let$ prompt_val, _ = prompt_val in
  prompt_field
  |> ui_outline
       ~label:name
       ~label_bottom:
         (prompt_val |> String.length |> Int.to_string |> Printf.sprintf " %s ")
  |>$ Ui.event_filter (fun event ->
    match event with
    | `Key (`Escape, _) ->
      onExit `Closed;
      `Handled
    | _ ->
      `Unhandled)
;;

(**This prompt will either *)
let general_prompt ?(focus = Focus.make ()) ?(char_count = false) ~show_prompt_var ui =
  let prompt_input = Lwd.var ("", 0) in
  let prompt_val = Lwd.get prompt_input in
  (*Build the ui so that it is either the prompt or nothing depending on whether show prompt is enabled*)
  let prompt_ui =
    let$* show_prompt_val = Lwd.get show_prompt_var in
    let prompt_ui =
      show_prompt_val
      |> Option.map @@ fun (label, pre_fill, on_exit) ->
         let on_exit result =
           Focus.release focus;
           show_prompt_var $= None;
           prompt_input $= ("", 0);
           on_exit result
         in
         (*we need focus because the base ui is rendering first and so *)
         Focus.request focus;
         (*prefill the prompt if we want to *)
         if prompt_input |> Lwd.peek |> fst == ""
         then prompt_input $= (pre_fill, pre_fill |> String.length);
         let prompt_field =
           W.zbox
             [
               W.string ~attr:A.(st underline) "                                       "
               |> Lwd.pure
             ; W.edit_field
                 prompt_val
                 ~on_change:(fun state -> Lwd.set prompt_input state)
                 ~on_submit:(fun (str, _) -> on_exit (`Finished str))
             ]
         in
         let$* prompt_val, _ = prompt_val in
         let$* focus = focus |> Focus.status in
         let label_bottom =
           if char_count
           then Some (prompt_val |> String.length |> Int.to_string)
           else None
         in
         prompt_field
         |> border_box ~label_top:label ?label_bottom
         |> clear_bg
         |>$ Ui.event_filter ~focus (fun event ->
           match event with
           | `Key (`Escape, _) ->
             on_exit `Closed;
             `Handled
           | _ ->
             `Unhandled)
    in
    prompt_ui |> Option.value ~default:(Ui.empty |> Lwd.pure)
  in
  (*Now that we have the prompt ui we layer it ontop of the normal ui using zbox.
    My hope is that by not directly nesting them this will allow the ui to not re-render when the prompt appears*)
  W.zbox [ ui; prompt_ui |> Lwd.map ~f:(Ui.resize ~pad:neutral_grav) ]
;;

let prompt_example =
  let show_prompt_var = Lwd.var None in
  let ui =
    Ui.vcat
      [
        W.string "hi this is my main ui"
      ; W.string "another line"
      ; W.string "another line"
      ; W.string
          "another linanother \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrste"
      ; W.string "another line"
      ; W.string
          "another linanother \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrste"
      ; W.string "another line"
      ; W.string "another line"
      ; W.string "another line"
      ; W.string "another line"
      ; W.string "another line"
      ; W.string
          "another \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrst"
      ]
    |> Ui.keyboard_area (fun x ->
      match x with
      | `ASCII 'p', _ ->
        Lwd.set show_prompt_var @@ Some ("hi prompt", "pre_fill", fun _ -> ());
        `Handled
      | _ ->
        `Unhandled)
    |> Lwd.pure
  in
  let prompt = general_prompt ~show_prompt_var ui in
  W.h_pane prompt (W.string "other side" |> Lwd.pure)
;;

(** horizontal rule, has no width by default but is very very wide so it should fill any space*)
let h_rule =
  W.string
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  |> Ui.resize ~w:0 ~sw:100
;;

(** A scroll area that won't scroll beyond it's limits
    WARN This is bugged! becasue of some issues with height stretching that's disabled *)
let scroll_area_intern ?beforesize ?(max = false) ?focus ~state ~change t =
  let open Nottui_widgets in
  let open Lwd_utils in
  let w_visible = ref (-1) in
  let w_total = ref (-1) in
  let h_visible = ref (-1) in
  let h_total = ref (-1) in
  let scroll position bound handle delta =
    let newPos = position + delta in
    let newPos = clampi newPos ~min:0 ~max:bound in
    if newPos <> position then handle newPos;
    `Handled
  in
  let focus_handler state_w state_h =
    let scroll_w =
      scroll state_w.position state_w.bound (fun position ->
        change `ActionV ({ state_w with position }, state_h))
    in
    let scroll_h =
      scroll state_h.position state_h.bound (fun position ->
        change `ActionH (state_w, { state_h with position }))
    in
    function
    | `Arrow `Left, [] ->
      scroll_w (-scroll_step)
    | `Arrow `Right, [] ->
      scroll_w (+scroll_step)
    | `Arrow `Up, [] ->
      scroll_h (-scroll_step)
    | `Arrow `Down, [] ->
      scroll_h (+scroll_step)
    | `Page `Up, [] ->
      scroll_h (-scroll_step * 8)
    | `Page `Down, [] ->
      scroll_h (+scroll_step * 8)
    | _ ->
      `Unhandled
  in
  let scroll_handler state_w state_h ~x:_ ~y:_ =
    let scroll_h =
      scroll state_h.position state_h.bound (fun position ->
        change `ActionH (state_w, { state_h with position }))
    in
    function
    | `Scroll `Up ->
      scroll_h (-scroll_step)
    | `Scroll `Down ->
      scroll_h (+scroll_step)
    | _ ->
      `Unhandled
  in
  Lwd.map2 t state ~f:(fun t (state_w, state_h) ->
    let tw, th = Ui.layout_width t, Ui.layout_height t in
    let mw, mh = if max then Some tw, Some th else None, None in
    t
    |> Ui.resize ~w:5 ~sw:1
    |> Ui.size_sensor (fun ~w ~h ->
      let sense v_spec v state total visible =
        let tchange =
          if !total <> v_spec
          then (
            total := v_spec;
            true)
          else false
        in
        let vchange =
          if !visible <> v
          then (
            visible := v;
            true)
          else false
        in
        if tchange || vchange
        then
          Some
            {
              state with
              visible = !visible
            ; total = !total
            ; bound = maxi 0 (!total - !visible)
            }
        else None
      in
      let w_update = sense tw w state_w w_total w_visible in
      let h_update = sense th h state_h h_total h_visible in
      match w_update, h_update with
      | Some w, Some h ->
        change `ContentBoth (w, h)
      | Some w, None ->
        change `ContentW (w, state_h)
      | None, Some h ->
        change `ContentH (state_w, h)
      | None, None ->
        ())
    |> Ui.shift_area state_w.position state_h.position
    (* |>Ui.join_y (Ui.atom (I.string A.empty (string_of_int state_w.visible))) *)
    (*TODO: make an alternative that has this already set*)
    |> Ui.mouse_area (scroll_handler state_w state_h)
    |> Ui.keyboard_area ?focus (focus_handler state_w state_h))
;;

let scroll_area ?focus ui =
  let state = Lwd.var (W.default_scroll_state, W.default_scroll_state) in
  ui |> scroll_area_intern ?focus ~change:(fun _ x -> state $= x) ~state:(Lwd.get state)
;;

let v_scroll_area ui =
  let state = Lwd.var W.default_scroll_state in
  ui |> W.vscroll_area ~change:(fun _ x -> state $= x) ~state:(Lwd.get state)
;;

(**This is a simple popup that can show ontop of *)
let popup ~show_popup_var ui =
  let popup_ui =
    let$* show_popup = Lwd.get show_popup_var in
    match show_popup with
    | Some (content, label) ->
      let prompt_field = content in
      prompt_field |>$ Ui.resize ~w:5 |> border_box ~label_top:label |> clear_bg
    | None ->
      Ui.empty |> Lwd.pure
  in
  W.zbox [ ui; popup_ui |>$ Ui.resize ~crop:neutral_grav ~pad:neutral_grav ]
;;

(**This is a simple popup that can show ontop of *)
let popup ~show_popup_var ui =
  let popup_ui =
    let$* show_popup = Lwd.get show_popup_var in
    match show_popup with
    | Some (content, label) ->
      let prompt_field = content in
      prompt_field |>$ Ui.resize ~w:5 |> border_box ~label_top:label |> clear_bg
    | None ->
      Ui.empty |> Lwd.pure
  in
  W.zbox [ ui; popup_ui |>$ Ui.resize ~crop:neutral_grav ~pad:neutral_grav ]
;;

(*A simple un_navigateable input field that only allows typing and deleting content. Try using edit_field for somethign that allows navigating within the text*)
let input_field ?(focus = Focus.make ()) start_state ~on_change ~on_submit =
  let update focus_h focus text =
    let content =
      Ui.atom @@ I.hcat [ I.string A.(st underline) (if text = "" then " " else text) ]
    in
    let handler = function
      | `ASCII 'U', [ `Ctrl ] ->
        on_change "";
        `Handled (* clear *)
      | `Escape, [] ->
        Focus.release focus_h;
        `Handled
      | `ASCII k, _ ->
        let text = text ^ String.make 1 k in
        on_change text;
        `Handled
      | `Backspace, _ ->
        let text =
          if text |> String.length > 0
          then String.sub text 0 (String.length text - 1)
          else text
        in
        on_change text;
        `Handled
      | `Enter, _ ->
        on_submit text;
        `Handled
      | _ ->
        `Unhandled
    in
    Ui.keyboard_area ~focus handler content
  in
  let node = Lwd.map2 ~f:(update focus) (Focus.status focus) start_state in
  node
;;

(*A size sensor that automatically updates the size variable*)
let simpleSizeSensor ~size_var ui =
  ui
  |> Ui.size_sensor (fun ~w ~h ->
    if Lwd.peek size_var <> (w, h) then Lwd.set size_var (w, h))
;;

(**selectable list item with a ui and some data *)
type 'a selectable_item = {
    data : 'a
  (**info attached to each ui elment in the list,  used for filtering and on_select callback *)
  ; ui : bool -> Ui.t
}

(** A simple widged to make an item selectable. Used in conjuction with selection_list_custom*)
let selectable_item ui is_focused =
  let height = Ui.layout_height ui in
  let prefix =
    if is_focused then I.char A.(bg A.blue) '>' 1 height else I.char A.empty ' ' 1 height
  in
  Ui.hcat [ prefix |> Ui.atom; ui ]
;;

(** Selection list that allows for custom handling of keyboard events.
    Scrolls when the selection reaches the lower third
    Only handles up and down keyboard events. Use [~custom_handler] to do handle confirming your selection and such *)
let selection_list_custom
  ?(focus = Focus.make ())
  ?(on_selection_change = fun _ -> ())
  ~custom_handler
  (items : 'a selectable_item list Lwd.t)
  =
  let selected_var = Lwd.var 0 in
  let selected_position = Lwd.var (0, 0) in
  (*handle selections*)
  let render_items =
    let$ focus = focus |> Focus.status
    and$ items = items
    and$ selected = Lwd.get selected_var in
    (* First ensure if our list has gotten shorter we haven't selected off the list*)
    (* We do this here to ensure that the selected var is updated before we render to avoid double rendering*)
    let max_selected = List.length items - 1 in
    if Int.min selected max_selected <> selected then selected_var $= max_selected;
    let selected = Lwd.peek selected_var in
    List.nth_opt items selected |> Option.iter (fun x -> on_selection_change x.data);
    items
    |> List.mapi (fun i x ->
      if selected == i
      then
        x.ui true
        |> Ui.transient_sensor (fun ~x ~y ~w ~h () ->
          if (x, y) <> Lwd.peek selected_position then selected_position $= (x, y))
      else x.ui false)
    |> Ui.vcat
    |> Ui.keyboard_area ~focus (function
      | `Arrow `Up, [] ->
        let selected = max (Lwd.peek selected_var - 1) 0 in
        selected_var $= selected;
        `Handled
      | `Arrow `Down, [] ->
        let selected =
          Int.max (min (Lwd.peek selected_var + 1) ((items |> List.length) - 1)) 0
        in
        selected_var $= selected;
        `Handled
      | a ->
        custom_handler items selected_var a)
  in
  let rendered_size_var = Lwd.var (0, 0) in
  (*Handle scrolling*)
  let scrollitems =
    let size_var = Lwd.var (0, 0) in
    let shift_amount =
      let$ selected = Lwd.get selected_var
      and$ size = Lwd.get size_var
      and$ length = items |>$ List.length
      and$ ren_size = Lwd.get rendered_size_var in
      (*portion of the total size of the element that is rendered*)
      let size_ratio =
        (ren_size |> snd |> float_of_int) /. (size |> snd |> float_of_int)
      in
      (*Tries to ensure that we start scrolling the list when we've selected about a third of the way down (using 3.0 causes weird jumping, so i use just less than )*)
      let offset = size_ratio *. ((length |> float_of_int) /. 2.9) in
      (*portion of the list that is behind the selection*)
      let list_ratio =
        ((selected |> float_of_int) +. offset) /. (length |> float_of_int)
      in
      (*if our position is further down the list than the portion that is shown we will shift by that amoumt *)
      Float.max (list_ratio -. size_ratio) 0.0 *. (size |> snd |> float_of_int)
      |> int_of_float
    in
    let$ items = render_items
    and$ shift_amount = shift_amount in
    items
    |> Ui.shift_area 0 shift_amount
    |> Ui.resize ~sh:1
    |> simpleSizeSensor ~size_var
    |> Ui.resize ~w:3 ~sw:1 ~h:0 ~mw:1000
    |> simpleSizeSensor ~size_var:rendered_size_var
  in
  scrollitems
;;

(** A filterable selectable list.

    This version allows you to implement custom handlers for keys and only provides functionality for moving up and down the list.

    For basic usage you likely want {!filterable_selection_list} which provides `Enter` and `Esc` handlers *)
let filterable_selection_list_custom
  ?(focus = Focus.make ())
  ~(filter_predicate : string -> 'a -> bool)
  ~custom_handler
  ~filter_text_var
  (items : 'a selectable_item list Lwd.var)
  =
  (*filter the list whenever the input changes*)
  let items =
    (* if we re-render we should always reset the selected list *)
    let items =
      let$ filter_text = filter_text_var |> Lwd.get
      and$ items = items |> Lwd.get in
      items |> List.filter (fun x -> filter_predicate filter_text x.data)
    in
    selection_list_custom
      ~focus
      ~custom_handler:(fun _ id x ->
        custom_handler (Lwd.observe items |> Lwd.quick_sample) id x)
      items
  in
  items
;;

(** Filterable selection list

    Allows filtering and selecting items in a list.
    Also handles shifting the list so that the selection dosen't go out of view
    @param ~filter_predicate Function called to deterimine if an items should be included
    @param ~on_confirm Called when user presses enter
    @param ?on_esc Called when user presses esc
    @param list_items List of items to be displayed/selected/filtered *)
let filterable_selection_list
  ?(focus = Focus.make ())
  ~filter_predicate
  ?(on_esc = fun _ -> ())
  ~on_confirm
  list_items
  =
  let filter_text_var = Lwd.var "" in
  let filter_text_ui =
    input_field
      ~focus
      (filter_text_var |> Lwd.get)
      ~on_change:(fun x -> filter_text_var $= x)
      ~on_submit:(fun _ -> ())
    |>$ Ui.resize ~w:3 ~sw:1 ~mw:10000
  in
  let list_ui =
    list_items
    |> filterable_selection_list_custom
         ~filter_predicate
         ~focus
         ~filter_text_var
         ~custom_handler:(fun items selected_var key ->
           match key with
           | `Enter, [] ->
             List.nth_opt items (Lwd.peek selected_var)
             |> Option.iter (fun item -> item.data |> on_confirm);
             `Handled
           | `Escape, [] ->
             List.nth_opt items (Lwd.peek selected_var)
             |> Option.iter (fun item -> item.data |> on_esc);
             `Handled
           | _ ->
             `Unhandled)
  in
  W.vbox
    [
      filter_text_ui
      (*Ensures the filter text box never expands beyond the size of the list elements*)
      |> border_box
    ; list_ui |> border_box
    ]
  |> border_box
;;
