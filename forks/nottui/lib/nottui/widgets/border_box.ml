(** Border box. Surrounds a Ui element in a border that can have labels, or appear when focused *)

open Notty
open Nottui_main
open Lwd_infix


let focusable
      ?pad_w 
      ?pad_h 
      ?label_top
      ?label_bottom
      ?border_attr 
      ?style
      ?focus_attr 
      ?focus_style
      ?(focus = Focus.make ())
      ?(on_key = fun _ -> `Unhandled)
      input
  =
  let$ ui = input 
  and$ focus_status = Focus.status focus in
      ui
      |> Ui.keyboard_area ~focus:focus_status on_key
      |> Ui.border
           ?pad_w
           ?pad_h
           ?label_top
           ?label_bottom
           ?attr:border_attr
           ?focus_attr
           ?style
           ?focus_style
;;

let box
      ?pad_w
      ?pad_h
      ?label_top
      ?label_bottom
      ?border_attr 
      ?style
      ?focus_attr 
      ?focus_style
      input
  =
  Lwd.map input ~f:(fun ui ->
      ui
      |> Ui.border
           ?pad_w
           ?pad_h
           ?label_top
           ?label_bottom
           ?attr:border_attr
           ?focus_attr
           ?style
           ?focus_style)
;;
