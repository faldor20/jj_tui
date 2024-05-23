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
  if String.length str <= len then str else String.sub str 0 (len - 3) ^ "..."
;;

(** top border*)
let outline_top attr w label =
  let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
  let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
  and label = if label |> I.width > w - 2 then I.empty else label |> I.hpad 1 0
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
let border_box_intern ?(label_top = I.empty) ?(label_bottom = I.empty) w h pad pad_w input
  =
  let vbar = I.uchar A.empty (Uchar.of_int 0x2502) 1 h in
  Ui.vcat
    [
      outline_top A.empty w label_top |> Ui.atom |> Ui.resize ~w:0;
      Ui.hcat
        [
          vbar |> Ui.atom;
          I.void pad_w 1 |> Ui.atom;
          input |> Ui.resize ~pad;
          I.void pad_w 1 |> Ui.atom;
          vbar |> Ui.atom;
        ];
      outline_bot A.empty w label_bottom |> Ui.atom |> Ui.resize ~w:0;
    ]
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
    @param input The input widget to be bordered. *)
let border_box
  ?(scaling = `Static)
  ?(pad = neutral_grav)
  ?(pad_w = 2)
  ?(pad_h = 1)
  ?label_top
  ?label_bottom
  input
  =
  let max_width, min_width, sw =
    match scaling with
    | `Static ->
      None, None, None
    (* This allows the input to expand to fill space*)
    | `Expand sw ->
      None, None, Some sw
    (* This allows the input to shrink to some minimum width*)
    | `Shrinkable (min_width, sw) ->
      Some (2 + (pad_w * 2)), Some min_width, Some sw
  in
  let size = Lwd.var (0, 0) in
  let layout_width = Lwd.var 0 in
  let input =
    let$ input = input in
    (*We need this later to determine the max with*)
    layout_width $= (input |> Ui.layout_width);
    input
    (*This lets us tell the input to be a flexible size*)
    |> Ui.resize ?w:min_width ?sw
    |> Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then Lwd.set size (w, h))
  in
  (*This is original width and height of the input before padding or anything *)
  let$* o_w, o_h = Lwd.get size in
  let w, h = o_w + (pad_w * 2), o_h + (pad_h * 2) in
  let h = h in
  let$ input = input in
  let bbox =
    border_box_intern
      ?label_top:(label_top |> Option.map (make_label (w - 2)))
      ?label_bottom:(label_bottom |> Option.map (make_label (w - 2)))
      w
      h
      pad
      pad_w
      input
  in
  (*If we want the input to be shrinkable we make it expandable, set it's width to something small and then set a max width for the whole box*)
  bbox |> Ui.resize ?mw:(max_width |> Option.map (fun x -> x + Lwd.peek layout_width))
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

(*========Prompt=======*)
let prompt onExit name =
  let prompt_input = Lwd.var ("", 0) in
  let prompt_val = Lwd.get prompt_input in
  let$* prompt_field =
    W.zbox
      [
        W.string ~attr:A.(st underline) "                                       "
        |> Lwd.pure;
        W.edit_field
          prompt_val
          ~on_change:(fun state -> Lwd.set prompt_input state)
          ~on_submit:(fun (str, _) -> onExit (`Finished str));
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

(*TODO: I should check that focus handle isn't being made eadh time*)
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
         let$* prompt_field =
           W.zbox
             [
               W.string ~attr:A.(st underline) "                                       "
               |> Lwd.pure;
               W.edit_field
                 prompt_val
                 ~on_change:(fun state -> Lwd.set prompt_input state)
                 ~on_submit:(fun (str, _) -> on_exit (`Finished str));
             ]
         in
         let$* prompt_val, _ = prompt_val in
         let$* focus = focus |> Focus.status in
         let label_bottom =
           if char_count
           then
             Some (prompt_val |> String.length |> Int.to_string |> Printf.sprintf " %s ")
           else None
         in
         prompt_field
         |> ui_outline ~label ?label_bottom
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

(**This is a simple popup that can show ontop of *)
let popup ~show_popup_var ui =
  let popup_ui =
    let$* show_popup = Lwd.get show_popup_var in
    match show_popup with
    | Some (content, label) ->
      let prompt_field = content in
      prompt_field |> Lwd.bind ~f:(ui_outline ~label)
    | None ->
      Ui.empty |> Lwd.pure
  in
  W.zbox [ ui; popup_ui |> Lwd.map ~f:(Ui.resize ~w:25 ~pad:neutral_grav) ]
;;

let prompt_example =
  let show_prompt_var = Lwd.var None in
  let ui =
    Ui.vcat
      [
        W.string "hi this is my main ui";
        W.string "another line";
        W.string "another line";
        W.string
          "another linanother \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrste";
        W.string "another line";
        W.string
          "another linanother \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrste";
        W.string "another line";
        W.string "another line";
        W.string "another line";
        W.string "another line";
        W.string "another line";
        W.string
          "another \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrst";
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

let scrollable ui =
  let scrollState = Lwd.var W.default_scroll_state in
  W.vscroll_area
    ~change:(fun _action state -> scrollState $= state)
    ~state:(Lwd.get scrollState)
    ui
;;
