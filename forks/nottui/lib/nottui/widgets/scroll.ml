(** Widgets to make a section of UI scrollable*)
open Nottui_main

open Shared
open Lwd_infix

module Internal = struct
  let scroll_step = 1

  type scroll_state =
    { position : int
    ; bound : int
    }

  let default_scroll_state = { position = 0; bound = 0 }

  (** Primative for implementing scrolling, should be avoided unless you actually have reason to be changing the scroll state *)
  let vscroll_area_intern ~state ~change t =
    let visible = ref (-1) in
    let total = ref (-1) in
    let scroll state delta =
      let position = state.position + delta in
      let position = clampi position ~min:0 ~max:state.bound in
      if position <> state.position then change `Action { state with position };
      `Handled
    in
    let focus_handler state = function
      (*| `Arrow `Left , _ -> scroll (-scroll_step) 0*)
      (*| `Arrow `Right, _ -> scroll (+scroll_step) 0*)
      | `Arrow `Up, [] -> scroll state (-scroll_step)
      | `Arrow `Down, [] -> scroll state (+scroll_step)
      | `Page `Up, [] -> scroll state (-scroll_step * 8)
      | `Page `Down, [] -> scroll state (+scroll_step * 8)
      | _ -> `Unhandled
    in
    let scroll_handler state ~x:_ ~y:_ = function
      | `Scroll `Up -> scroll state (-scroll_step)
      | `Scroll `Down -> scroll state (+scroll_step)
      | _ -> `Unhandled
    in
    Lwd.map2 t state ~f:(fun t state ->
      let tmh = Ui.layout_max_height t in
      t
      |> Ui.shift_area 0 state.position
      |> Ui.resize ~h:0 ~sh:1 ~mh:10000
      |> Ui.size_sensor (fun ~w:_ ~h ->
        let tchange =
          if !total <> (Ui.layout_spec t).Ui.h
          then (
            total := (Ui.layout_spec t).Ui.h;
            true)
          else false
        in
        let vchange =
          if !visible <> h
          then (
            visible := h;
            true)
          else false
        in
        if tchange || vchange
        then change `Content { state with bound = maxi 0 (!total - !visible) })
      |> Ui.mouse_area (scroll_handler state)
      |> Ui.keyboard_area (focus_handler state)
      (*restore original max height*)
      |> Ui.resize ~mh:tmh)
  ;;

  let scroll_area_intern ?focus ~state ~change t =
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
      | `Arrow `Left, [] -> scroll_w (-scroll_step)
      | `Arrow `Right, [] -> scroll_w (+scroll_step)
      | `Arrow `Up, [] -> scroll_h (-scroll_step)
      | `Arrow `Down, [] -> scroll_h (+scroll_step)
      | `Page `Up, [] -> scroll_h (-scroll_step * 8)
      | `Page `Down, [] -> scroll_h (+scroll_step * 8)
      | _ -> `Unhandled
    in
    let scroll_handler state_w state_h ~x:_ ~y:_ =
      let scroll_h =
        scroll state_h.position state_h.bound (fun position ->
          change `ActionH (state_w, { state_h with position }))
      in
      function
      | `Scroll `Up -> scroll_h (-scroll_step)
      | `Scroll `Down -> scroll_h (+scroll_step)
      | _ -> `Unhandled
    in
    Lwd.map2 t state ~f:(fun t (state_w, state_h) ->
      let tw, th = Ui.layout_width t, Ui.layout_height t in
      let tmw, tmh = Ui.layout_max_width t, Ui.layout_max_height t in
      (* let mw, mh = if max then Some tw, Some th else None, None in *)
      t
      |> Ui.resize ~w:0 ~sw:1 ~h:0 ~sh:1 ~mw:10000 ~mh:10000
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
          then Some { state with bound = maxi 0 (!total - !visible) }
          else None
        in
        let w_update = sense tw w state_w w_total w_visible in
        let h_update = sense th h state_h h_total h_visible in
        match w_update, h_update with
        | Some w, Some h -> change `ContentBoth (w, h)
        | Some w, None -> change `ContentW (w, state_h)
        | None, Some h -> change `ContentH (state_w, h)
        | None, None -> ())
      |> Ui.mouse_area (scroll_handler state_w state_h)
      |> Ui.keyboard_area ?focus (focus_handler state_w state_h)
      (*restore original mw*)
      |> Ui.resize ~mw:tmw ~mh:tmh)
  ;;
end

open Internal

let v_area ui =
  let state = Lwd.var Internal.default_scroll_state in
  ui
  |> Internal.vscroll_area_intern ~change:(fun _ x -> state $= x) ~state:(Lwd.get state)
;;

let area ?focus ui =
  let state = Lwd.var (Internal.default_scroll_state, Internal.default_scroll_state) in
  ui
  |> Internal.scroll_area_intern
       ?focus
       ~change:(fun _ x -> state $= x)
       ~state:(Lwd.get state)
;;

let infinite_area ?(offset = 0, 0) t =
  let offset = Lwd.var offset in
  let scroll d_x d_y =
    let s_x, s_y = Lwd.peek offset in
    let s_x = maxi 0 (s_x + d_x) in
    let s_y = maxi 0 (s_y + d_y) in
    Lwd.set offset (s_x, s_y);
    `Handled
  in
  let focus_handler = function
    | `Arrow `Left, [] -> scroll (-scroll_step) 0
    | `Arrow `Right, [] -> scroll (+scroll_step) 0
    | `Arrow `Up, [] -> scroll 0 (-scroll_step)
    | `Arrow `Down, [] -> scroll 0 (+scroll_step)
    | `Page `Up, [] -> scroll 0 (-scroll_step * 8)
    | `Page `Down, [] -> scroll 0 (+scroll_step * 8)
    | _ -> `Unhandled
  in
  let scroll_handler ~x:_ ~y:_ = function
    | `Scroll `Up -> scroll 0 (-scroll_step)
    | `Scroll `Down -> scroll 0 (+scroll_step)
    | _ -> `Unhandled
  in
  Lwd.map2 t (Lwd.get offset) ~f:(fun t (s_x, s_y) ->
    t
    |> Ui.shift_area s_x s_y
    |> Ui.mouse_area scroll_handler
    |> Ui.keyboard_area focus_handler)
;;
