open Notty
open Nottui
module W = Nottui_widgets

let neutral_grav = Gravity.make ~h:`Neutral ~v:`Neutral
let make_even num = num + (num mod 2 * 1)

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
  let width = Ui.layout_width input in
  let height = Ui.layout_height input in
  let edit =
    Ui.zcat
      [
        I.char A.empty ' ' (width + pad_h) (height + pad_v) |> Ui.atom;
        input |> Ui.resize ~pad |> pad_edge (pad_h / 2) (pad_v / 2) pad;
      ]
  in
  let width = Ui.layout_width edit |> make_even in
  let h_border = String.init width (fun _ -> '=') |> W.string in
  let v_body =
    Ui.vcat
      [
        Ui.zcat [ h_border; W.string label |> Ui.resize ~pad |> pad_edge 1 0 pad ];
        edit;
        h_border;
      ]
  in
  let p = I.string A.empty "|" in
  let v_border =
    I.vcat (List.init (v_body |> Ui.layout_height) (fun _ -> p)) |> Ui.atom
  in
  Ui.hcat [ v_border; v_body; v_border ]
;;
