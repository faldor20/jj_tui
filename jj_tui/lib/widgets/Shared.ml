
open Notty
open Nottui
open Lwd_infix
open! Util

(**A size sensor that automatically updates the size variable*)
let simpleSizeSensor ~size_var ui =
  ui
  |> Ui.size_sensor (fun ~w ~h ->
    if Lwd.peek size_var <> (w, h) then Lwd.set size_var (w, h))
;;

(*A simple un_navigateable input field that only allows typing and deleting content. Try using edit_field for something that allows navigating within the text*)
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

