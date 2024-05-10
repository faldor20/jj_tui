open Notty
open Nottui
open Lwd_infix
module W = Nottui_widgets

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

let border_box ?(pad = neutral_grav) ?(pad_h = 4) ?(pad_v = 2) ?(label = "") input =
  let width = Ui.layout_width input |> make_even in
  let height = Ui.layout_height input in
  let edit =
    Ui.zcat
      [
        I.char A.empty ' ' (width + pad_h) (height + pad_v) |> Ui.atom;
        input |> Ui.resize ~pad |> pad_edge (pad_h / 2) (pad_v / 2) pad;
      ]
  in
  let p = I.uchar A.empty upPipe 1 1 in
  let v_border =
    I.vcat (List.init (edit |> Ui.layout_height) (fun _ -> p))
    |> Ui.atom
    |> Ui.resize ~pad:neutral_grav
  in
  let h_body = Ui.hcat [ v_border; edit; v_border ] in
  let width = Ui.layout_width h_body in
  let v_body =
    Ui.vcat
      [
        Ui.zcat
          [
            make_top width |> Ui.atom;
            W.string label |> Ui.resize ~pad |> pad_edge 1 0 pad;
          ];
        h_body;
        make_bot width |> Ui.atom;
      ]
  in
  v_body
;;

let grid xxs = xxs |> List.map I.hcat |> I.vcat

(** image outline creator*)
let outline attr i =
  let w, h = I.(width i, height i) in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
  and vbar = I.uchar attr (Uchar.of_int 0x2502) 1 h in
  let a, b, c, d = chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570 in
  grid [ [ a; hbar; b ]; [ vbar; i; vbar ]; [ d; hbar; c ] ]
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
  let w, h = Ui.(layout_width ui, layout_height ui) in
  let outline =
    label_outline_strings
      ~label_bottom
      ~label
      ~attr
      (I.char A.empty ' ' (w + pad_w) (h + pad_h))
    |> Ui.atom
  in
  Ui.zcat [ outline; ui |> Ui.resize ~pad |> pad_edge (pad_w / 2) (pad_h / 2) pad ]
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
  |> Ui.event_filter (fun event ->
    match event with
    | `Key (`Escape, _) ->
      onExit `Closed;
      `Handled
    | _ ->
      `Unhandled)
;;

(*TODO: I should check that focus handle isn't being made eadh time*)
(**This prompt will either *)
let general_prompt
  ?(focus = Focus.make ())
  ?(char_count = false)
  ~show_prompt:show_prompt_var
  name
  ui
  =
  let prompt_input = Lwd.var ("", 0) in
  let prompt_val = Lwd.get prompt_input in
  (*Build the ui so that it is either the prompt or nothing depending on whether show prompt is enabled*)
  let prompt_ui =
    let$* show_prompt_val = Lwd.get show_prompt_var in
    let prompt_ui =
      show_prompt_val
      |> Option.map @@ fun (pre_fill, on_exit) ->
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
         let$ focus = focus |> Focus.status in
         let label_bottom =
           if char_count
           then
             Some (prompt_val |> String.length |> Int.to_string |> Printf.sprintf " %s ")
           else None
         in
         prompt_field
         |> ui_outline ~label:name ?label_bottom
         |> Ui.event_filter ~focus (fun event ->
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
  let show_prompt = Lwd.var None in
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
        Lwd.set show_prompt @@ Some ("pre_fill", fun _ -> ());
        `Handled
      | _ ->
        `Unhandled)
    |> Lwd.pure
  in
  let prompt = general_prompt ~show_prompt "this is a prompt" ui in
  W.h_pane prompt (W.string "other side" |> Lwd.pure)
;;
