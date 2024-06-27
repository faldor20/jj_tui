open Notty
open Nottui_main
open Lwd_infix
include Shared
include Nottui_widgets
module Old = Old
module Wip = Wip
module Box = Border_box
module Overlay = Overlays
module Scroll = Scroll

module Lists = struct
  include Selection_list
  include Lists
end

module W = Nottui_widgets

(** Shows the size of the ui provided. Useful for debugging*)
let size_logger ui =
  let size = Lwd.var (-1, -1) in
  W.vbox
    [ (size |> Lwd.get |>$ fun (w, h) -> W.fmt "w:%d,h:%d" w h)
    ; ui
      |>$ Ui.size_sensor (fun ~w ~h ->
        if Lwd.peek size <> (w, h) then Lwd.set size (w, h))
    ]
;;

(** horizontal rule, has no width by default but is very very wide so it should fill any space*)
let h_rule =
  W.string
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  |> Ui.resize ~w:0 ~sw:100
;;

(** Tab view, where exactly one element of [l] is shown at a time. Naviagted using number keys on the keyboard *)
let keyboard_tabs (tabs : (string * (unit -> Ui.t Lwd.t)) list) : Ui.t Lwd.t =
  (* Gets an int that a char represents*)
  let char_to_int c =
    if c >= '0' && c <= '9' then Some (int_of_char c - int_of_char '0') else None
  in
  match tabs with
  | [] -> Lwd.return Ui.empty
  | _ ->
    let cur = Lwd.var 0 in
    let$* idx_sel = Lwd.get cur in
    let _, f = List.nth tabs idx_sel in
    let tab_bar =
      tabs
      |> List.mapi (fun i (s, _) ->
        let attr = if i = idx_sel then A.(st underline) else A.empty in
        let tab_annot = W.printf ~attr "%s [%d]" s (i + 1) in
        tab_annot)
      |> List.intersperse ~sep:(W.string " | ")
      |> Ui.hcat
      |> Ui.resize ~sw:1 ~mw:10000
      |> Lwd.pure
      |> Box.box ~pad_w:1 ~pad_h:0
    in
    W.vbox [ tab_bar; f () ]
    |>$ Ui.keyboard_area (function
      | `ASCII key, _ ->
        key
        |> char_to_int
        |> Option.map (fun i ->
          if i >= 1 && i <= List.length tabs
          then (
            cur $= i - 1;
            `Handled)
          else `Unhandled)
        |> Option.value ~default:`Unhandled
      | _ -> `Unhandled)
;;
