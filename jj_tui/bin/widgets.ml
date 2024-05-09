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

(*========Prompt=======*)
let prompt onExit name =
  let prompt_input = Lwd.var ("", 0) in
  let$ prompt_field =
    W.zbox
      [
        W.string ~attr:A.(st underline) "                                       "
        |> Lwd.pure;
        W.edit_field
          (Lwd.get prompt_input)
          ~on_change:(fun state -> Lwd.set prompt_input state)
          ~on_submit:(fun (str, _) -> onExit (`Finished str));
      ]
  in
  prompt_field
  |> border_box ~pad:Gravity.default ~label:name
  |> Ui.event_filter (fun event ->
    match event with
    | `Key (`Escape, _) ->
      onExit `Closed;
      `Handled
    | _ ->
      `Unhandled)
;;

