open Notty
open Nottui
open Lwd_infix
open! Util
open! Widgets_citty
include Box_widget
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

let monitor ui =
  ui |> Ui.layout_spec |> Ui.pp_layout_spec Format.str_formatter;
  let str = Format.flush_str_formatter () in
  let log =
    str |> String.split_on_char '\n' |> List.map (I.string A.empty) |> I.vcat |> Ui.atom
  in
  Ui.vcat [ ui; log ]
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
    |> Ui.resize ~w:5 ~sw:1 ~h:2 ~sh:1
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

(*A simple un_navigateable input field that only allows typing and deleting content. Try using edit_field for somethign that allows navigating within the text*)
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

(*A size sensor that automatically updates the size variable*)
let simpleSizeSensor ~size_var ui =
  ui
  |> Ui.size_sensor (fun ~w ~h ->
    if Lwd.peek size_var <> (w, h) then Lwd.set size_var (w, h))
;;

(**selectable list item with a ui and some data *)
type 'a selectable_item = {
    data : 'a
  (**info attached to each ui elment in the list,  used for filtering and on_select callback *)
  ; ui : bool -> Ui.t
}

(** A simple widged to make an item selectable. Used in conjuction with selection_list_custom*)
let selectable_item ui is_focused =
  let height = Ui.layout_height ui in
  let prefix =
    if is_focused then I.char A.(bg A.blue) '>' 1 height else I.char A.empty ' ' 1 height
  in
  Ui.hcat [ prefix |> Ui.atom; ui ]
;;

(** Selection list that allows for custom handling of keyboard events.
    Scrolls when the selection reaches the lower third
    Only handles up and down keyboard events. Use [~custom_handler] to do handle confirming your selection and such *)
let selection_list_custom
  ?(focus = Focus.make ())
  ?(on_selection_change = fun _ -> ())
  ~custom_handler
  (items : 'a selectable_item list Lwd.t)
  =
  let selected_var = Lwd.var 0 in
  let selected_position = Lwd.var (0, 0) in
  (*handle selections*)
  let render_items =
    let$ focus = focus |> Focus.status
    and$ items, selected =
      (* This doesn't depend on changes in focus so we run it with just the items and selected*)
      let$ items = items
      and$ selected = Lwd.get selected_var in
      (* First ensure if our list has gotten shorter we haven't selected off the list*)
      (* We do this here to ensure that the selected var is updated before we render to avoid double rendering*)
      let max_selected = Int.max 0 (List.length items - 1) in
      if Int.min selected max_selected <> selected then selected_var $= max_selected;
      let selected = Lwd.peek selected_var in
      List.nth_opt items selected |> Option.iter (fun x -> on_selection_change x.data);
      items, selected
    in
    (*Ui.vcat can be a little weird when the*)
    items
    |> List.mapi (fun i x ->
      if selected == i
      then
        x.ui true
        |> Ui.transient_sensor (fun ~x ~y ~w:_ ~h:_ () ->
          if (x, y) <> Lwd.peek selected_position then selected_position $= (x, y))
      else x.ui false)
    |> Ui.vcat
    |> Ui.keyboard_area ~focus (function
      | `Arrow `Up, [] ->
        let selected = max (Lwd.peek selected_var - 1) 0 in
        selected_var $= selected;
        `Handled
      | `Arrow `Down, [] ->
        let selected =
          Int.max (min (Lwd.peek selected_var + 1) ((items |> List.length) - 1)) 0
        in
        selected_var $= selected;
        `Handled
      | a ->
        custom_handler items selected_var a)
  in
  let rendered_size_var = Lwd.var (0, 0) in
  (*Handle scrolling*)
  let scrollitems =
    let size_var = Lwd.var (0, 0) in
    let shift_amount =
      let$ selected = Lwd.get selected_var
      and$ size = Lwd.get size_var
      and$ length = items |>$ List.length
      and$ ren_size = Lwd.get rendered_size_var in
      (*portion of the total size of the element that is rendered*)
      let size_ratio =
        (ren_size |> snd |> float_of_int) /. (size |> snd |> float_of_int)
      in
      (*Tries to ensure that we start scrolling the list when we've selected about a third of the way down (using 3.0 causes weird jumping, so i use just less than )*)
      let offset = size_ratio *. ((length |> float_of_int) /. 2.9) in
      (*portion of the list that is behind the selection*)
      let list_ratio =
        ((selected |> float_of_int) +. offset) /. (length |> float_of_int)
      in
      (*if our position is further down the list than the portion that is shown we will shift by that amoumt *)
      Float.max (list_ratio -. size_ratio) 0.0 *. (size |> snd |> float_of_int)
      |> int_of_float
    in
    let$ items = render_items
    and$ shift_amount = shift_amount in
    items
    |> Ui.shift_area 0 shift_amount
    |> Ui.resize ~sh:1
    |> simpleSizeSensor ~size_var
    |> Ui.resize ~w:3 ~sw:1 ~h:0
    |> simpleSizeSensor ~size_var:rendered_size_var
  in
  scrollitems
;;

(** A filterable selectable list.

    This version allows you to implement custom handlers for keys and only provides functionality for moving up and down the list.

    For basic usage you likely want {!filterable_selection_list} which provides `Enter` and `Esc` handlers *)
let filterable_selection_list_custom
  ?(focus = Focus.make ())
  ~(filter_predicate : string -> 'a -> bool)
  ~custom_handler
  ~filter_text_var
  (items : 'a selectable_item list Lwd.t)
  =
  (*filter the list whenever the input changes*)
  let items =
    (* if we re-render we should always reset the selected list *)
    let items =
      let$ filter_text = filter_text_var |> Lwd.get
      and$ items = items in
      items |> List.filter (fun x -> filter_predicate filter_text x.data)
    in
    selection_list_custom
      ~focus
      ~custom_handler:(fun _ id x ->
        custom_handler (Lwd.observe items |> Lwd.quick_sample) id x)
      items
  in
  items
;;

(** Filterable selection list

    Allows filtering and selecting items in a list.
    Also handles shifting the list so that the selection dosen't go out of view
    @param ~filter_predicate Function called to deterimine if an items should be included
    @param ~on_confirm Called when user presses enter
    @param ?on_esc Called when user presses esc
    @param list_items List of items to be displayed/selected/filtered *)
let filterable_selection_list
  ?(focus = Focus.make ())
  ~filter_predicate
  ?(on_esc = fun _ -> ())
  ~on_confirm
  list_items
  =
  let filter_text_var = Lwd.var "" in
  let filter_text_ui =
    input_field
      ~focus
      (filter_text_var |> Lwd.get)
      ~on_change:(fun x -> filter_text_var $= x)
      ~on_submit:(fun _ -> ())
    |>$ Ui.resize ~w:3 ~sw:1 ~mw:10000
  in
  let list_ui =
    list_items
    |> filterable_selection_list_custom
         ~filter_predicate
         ~focus
         ~filter_text_var
         ~custom_handler:(fun items selected_var key ->
           match key with
           | `Enter, [] ->
             List.nth_opt items (Lwd.peek selected_var)
             |> Option.iter (fun item -> item.data |> on_confirm);
             `Handled
           | `Escape, [] ->
             List.nth_opt items (Lwd.peek selected_var)
             |> Option.iter (fun item -> item.data |> on_esc);
             `Handled
           | _ ->
             `Unhandled)
  in
  W.vbox
    [
      filter_text_ui
      (*Ensures the filter text box never expands beyond the size of the list elements*)
      |> border_box
    ; list_ui |> border_box
    ]
  |> border_box
;;

(** [on_focus f ui]

    Transforms the Ui with function [f] if the Ui is focused *)
let on_focus f ui = if ui |> Ui.has_focus then ui |> f else ui
