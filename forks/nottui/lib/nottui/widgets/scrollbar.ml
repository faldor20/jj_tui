(** Shared scrollbar component for scrollable widgets.

    The higher-level scroll containers in [scroll.ml] own the event handling and
    viewport measurements. This module only turns that state into a visual
    scrollbar so the rendering stays consistent across vertical and horizontal
    scroll areas. *)
open Nottui_main
open Notty

(** Scroll position plus its maximum bound for a single axis. *)
type state =
  { position : int
  ; bound : int
  }

(** Scrollbar axis selector. *)
type axis = [ `Horizontal | `Vertical ]

(* Axis-specific glyphs, styling, and packing behaviour. This keeps the actual
   drawing code generic while still producing axis-appropriate output. *)
type axis_config =
  { start_glyph : Uchar.t
  ; end_glyph : Uchar.t
  ; track_glyph : Uchar.t
  ; track_attr : A.t
  ; active_attr : A.t
  ; pack : I.t list -> I.t
  ; resize : Ui.t -> Ui.t
  }

let empty_glyph = Notty.make_uchar " "
let up_glyph = Notty.make_uchar "▲"
let down_glyph = Notty.make_uchar "▼"
let rectangle_thumb_glyph = Notty.make_uchar "■"
let round_thumb_glyph = Notty.make_uchar "●"
let vertical_track_glyph = Notty.make_uchar "│"
let left_glyph = Notty.make_uchar "◀"
let right_glyph = Notty.make_uchar "▶"
let horizontal_track_glyph = Notty.make_uchar "─"

(* Respect the global Nottui rendering config so all scrollbars switch thumb
   style together instead of each widget carrying its own toggle. *)
let thumb_glyph () =
  match Ui.global_config.scrollbar_thumb_style with
  | `Rectangle -> rectangle_thumb_glyph
  | `Round -> round_thumb_glyph
;;

let axis_config = function
  | `Vertical ->
    { start_glyph = up_glyph
    ; end_glyph = down_glyph
    ; track_glyph = vertical_track_glyph
    ; track_attr = A.(fg lightblack)
    ; active_attr = A.(fg white ++ st bold)
    ; pack = I.vcat
    ; resize = Ui.resize ~w:1 ~sw:0 ~h:0 ~sh:1
    }
  | `Horizontal ->
    { start_glyph = left_glyph
    ; end_glyph = right_glyph
    ; track_glyph = horizontal_track_glyph
    ; track_attr = A.(fg lightblack)
    ; active_attr = A.(fg white ++ st bold)
    ; pack = I.hcat
    ; resize = Ui.resize ~w:0 ~sw:1 ~h:1 ~sh:0
    }
;;

let indicator_glyph (axis : axis_config) ~position ~bound ~visible index =
  if bound <= 0 || visible <= 0
  then empty_glyph
  else (
    let has_before = position > 0 in
    let has_after = position < bound in
    let thumb_index =
      if visible <= 1 then 0 else position * (visible - 1) / (max 1 bound)
    in
    let thumb = thumb_glyph () in
    if visible = 1
    then if has_after then axis.end_glyph else if has_before then axis.start_glyph else thumb
    else if index = 0 && has_before
    then axis.start_glyph
    else if index = visible - 1 && has_after
    then axis.end_glyph
    else if index = thumb_index
    then thumb
    else axis.track_glyph)
;;

let indicator_attr (axis : axis_config) ~focused glyph =
  if Uchar.equal glyph axis.track_glyph
  then axis.track_attr
  else if Uchar.equal glyph empty_glyph
  then A.empty
  else if focused
  then axis.active_attr
  else axis.track_attr
;;

let render axis ~focused ~state ~visible =
  let axis = axis_config axis in
  if visible <= 0
  then Ui.empty
  else (
    let image =
      List.init visible (fun index ->
        let glyph = indicator_glyph axis ~position:state.position ~bound:state.bound ~visible index in
        I.uchar (indicator_attr axis ~focused glyph) glyph 1 1)
      |> axis.pack
    in
    Ui.atom image |> axis.resize)
;;
