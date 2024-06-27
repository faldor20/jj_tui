(*open Nottui
  open Lwd_infix

  let selected_item = Lwd.var 0
  let selectable_lwd = Lwd.get selected_item

  let selectable ?(focus = Focus.make ()) id =
  let selected =
  let$ focus = Focus.status focus in
  (*set the focus watcher*)
  let focused = Focus.has_focus focus in
  if focused && Lwd.peek selected_item <> id then Some id else None
  in
  let a =
  let$ focus = Focus.status focus in
  (*set the focus watcher*)
  let focused = Focus.has_focus focus in
  if focused && Lwd.peek selected_item <> id then selected_item $= id;
  W.printf "hi %d focused?:%b" id focused
  |> Ui.keyboard_area ~focus (fun _ -> `Unhandled)
  in
  a, selected
  ;;

  let main =
  let ui, selection = [ 1; 2; 3 ] |> List.map selectable |> List.to_seq |> Seq.unzip in
  let se =
  selection
  |> List.of_seq
  |> Lwd_seq.of_list
  |> Lwd.pure
  |> Lwd_seq.lift
  |> Lwd_seq.fold_monoid
  (fun x -> x)
  (None, fun x y -> if x |> Option.is_some then x else y)
  in
  W.hbox [ W.vbox (ui |> List.of_seq); se |>$ Option.value ~default:0 |>$ W.int ]
  ;;
*)

open Nottui
open Lwd_infix

let selected_item = Lwd.var 0
let selectable_lwd = Lwd.get selected_item

let selectable ?(focus = Focus.make ()) id =
  let a =
    let$ focus = Focus.status focus in
    (*set the focus watcher*)
    let focused = Focus.has_focus focus in
    if focused && Lwd.peek selected_item <> id then selected_item $= id;
    W.printf "hi %d focused?:%b" id focused
    |> Ui.keyboard_area ~focus (fun _ -> `Unhandled)
  in
  a |> Lwd.fix ~wrt:selectable_lwd
;;

let main =
  W.hbox
    [ W.vbox ([ 1; 2; 3 ] |> List.map selectable) |> Lwd.fix ~wrt:selectable_lwd
    ; (let$ selected_item = selectable_lwd in
       selected_item |> W.int)
      |> Lwd.fix ~wrt:selectable_lwd
    ]
  |> Lwd.fix ~wrt:selectable_lwd
;;
