(** Border box. Surrounds a Ui element in a border that can have labels, or appear when focused *)

open Notty
open Nottui_main
open Lwd_infix

let with_border_attr
      ?(pad = Gravity.make ~h:`Neutral ~v:`Neutral)
      ?(pad_w = 2)
      ?(pad_h = 1)
      ?label_top
      ?label_bottom
      get_border
      input
  =
  Lwd.map2 input get_border ~f:(fun ui attr ->
      ui
      |> Ui.resize ~pad
      |> Ui.border ~pad_w ~pad_h ?label_top ?label_bottom ~attr)
;;

let focusable
      ?pad
      ?(pad_w = 2)
      ?(pad_h = 1)
      ?label_top
      ?label_bottom
      ?(border_attr = A.empty)
      ?(focus_attr = (A.fg A.blue))
      ?style
      ?(focus_style=Ui.Border.unicode_bold)
      ?(focus = Focus.make ())
      ?(on_key = fun _ -> `Unhandled)
      input
  =
  let focus_status = Focus.status focus in
  let$ ui = input 
  and$ focus_status = Focus.status focus in
      ui
      |> Ui.keyboard_area ~focus:focus_status on_key
      |> Ui.resize ?pad
      |> Ui.border
           ~pad_w
           ~pad_h
           ?label_top
           ?label_bottom
           ~attr:border_attr
           ~focus_attr:(focus_attr)
           ?style
           ~focus_style
;;

let box
      ?pad
      ?pad_w
      ?pad_h
      ?label_top
      ?label_bottom
      ?(border_attr = A.empty)
      ?focus_attr
      ?style
      ?focus_style
      input
  =
  Lwd.map input ~f:(fun ui ->
      ui
      |> Ui.resize ?pad
      |> Ui.border
           ?pad_w
           ?pad_h
           ?label_top
           ?label_bottom
           ~attr:border_attr
           ?focus_attr
           ?style
           ?focus_style)
;;

let static
      ?(pad = Gravity.make ~h:`Neutral ~v:`Neutral)
      ?(pad_w = 2)
      ?(pad_h = 1)
      ?label_top
      ?label_bottom
      ?(border_attr = A.empty)
      ui
  =
  ui
  |> Ui.resize ~pad
  |> Ui.border
       ~pad_w
       ~pad_h
       ?label_top
       ?label_bottom
       ~attr:border_attr
;;
