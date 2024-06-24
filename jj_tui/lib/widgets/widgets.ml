open Notty
open Nottui
open Lwd_infix
open! Util
open! Widgets_citty
include Box_widget
include Shared
include Selection_list
module Wip = Wip

let neutral_grav = Gravity.make ~h:`Neutral ~v:`Neutral
let make_even num = num + (num mod 2 * 1)
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

(** Shows the size of the ui provided. Useful for debugging*)
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
  |> Lwd.pure
  |> border_box
       ~label_top:name
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
           Focus.release_reversable focus;
           show_prompt_var $= None;
           prompt_input $= ("", 0);
           on_exit result
         in
         (*we need focus because the base ui is rendering first and so *)
         Focus.request_reversable focus;
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
         let$* prompt_val, _ = prompt_val
         and$ focus_status = focus |> Focus.status in
         let label_bottom =
           if char_count
           then Some (prompt_val |> String.length |> Int.to_string)
           else None
         in
         prompt_field
         |> border_box_focusable ~focus ~label_top:label ?label_bottom
         |> clear_bg
         |>$ Ui.event_filter ~focus:focus_status (fun event ->
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

let scroll_area_intern ?focus ~state ~change t =
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
    (* let mw, mh = if max then Some tw, Some th else None, None in *)
    t
    |> Ui.resize ~w:0 ~sw:1 ~h:0 ~sh:1
    |> Ui.shift_area state_w.position state_h.position
    (* |>Ui.join_y (Ui.atom (I.string A.empty (string_of_int state_w.visible))) *)
    (*TODO: make an alternative that has this already set*)
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
    |> Ui.mouse_area (scroll_handler state_w state_h)
    |> Ui.keyboard_area ?focus (focus_handler state_w state_h))
;;

(** A scroll area that allows keyboard scrolling in both x and y directions*)
let scroll_area ?focus ui =
  let state = Lwd.var (W.default_scroll_state, W.default_scroll_state) in
  ui |> scroll_area_intern ?focus ~change:(fun _ x -> state $= x) ~state:(Lwd.get state)
;;

let v_scroll_area ui =
  let state = Lwd.var W.default_scroll_state in
  ui |> W.vscroll_area ~change:(fun _ x -> state $= x) ~state:(Lwd.get state)
;;

(**This is a simple popup that can show ontop of other ui elements *)
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

(** [on_focus f ui]

    Transforms the Ui with function [f] if the Ui is focused *)
let on_ui_focus f ui = if ui |> Ui.has_focus then ui |> f else ui

let on_focus ~focus f ui =
  Lwd.map2 ui (focus |> Focus.status) ~f:(fun ui focus ->
    if focus |> Focus.has_focus then ui |> f else ui)
;;

let is_focused ~focus f ui =
  Lwd.map2 ui (focus |> Focus.status) ~f:(fun ui focus -> f ui (focus |> Focus.has_focus))
;;

(** Gets an int that a char represents*)
let char_to_int c =
  if c >= '0' && c <= '9' then Some (int_of_char c - int_of_char '0') else None
;;

(** Tab view, where exactly one element of [l] is shown at a time. *)
let mouse_tabs (tabs : (string * (unit -> Ui.t Lwd.t)) list) : Ui.t Lwd.t =
  match tabs with
  | [] ->
    Lwd.return Ui.empty
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
      |> Base.List.intersperse ~sep:(W.string " | ")
      |> Ui.hcat
      |> Ui.resize ~sw:1 ~mw:10000
      |> Lwd.pure
      |> border_box ~pad_w:1 ~pad_h:0
    in
    W.vbox [ tab_bar; f () ]
        |>$ Ui.keyboard_area (function
          | `ASCII key, _ ->
            key
            |> char_to_int
            |> Option.map (fun i ->
              if i >= 1 && i <= List.length tabs
              then (
                cur $= i-1;
                `Handled)
              else `Unhandled)
            |> Option.value ~default:`Unhandled
          | _ ->
            `Unhandled)
;;
