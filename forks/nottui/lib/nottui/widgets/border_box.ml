(** Border box. Surrounds a Ui element in a border that can have labels, or appear when focused *)

open Notty
open Nottui_main
open Lwd_infix

(*------ Internal/Private----*)
module Internal = struct
  let neutral_grav = Gravity.make ~h:`Neutral ~v:`Neutral

  module W = Nottui_widgets

  (** Truncate a string to a given length, adding an ellipsis if truncated. *)
  let truncate_string len str =
    if String.length str > len
    then if len <= 3 then "" else String.sub str 0 (len - 3) ^ "..."
    else str
  ;;

  (** top border*)
  let outline_top attr w label =
    let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
    let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
    and label = if label |> I.width > w - 2 then I.empty else label |> I.hpad 2 0
    and a, b = chr 0x256d, chr 0x256e in
    I.zcat [ label; I.hcat [ a; hbar; b ]; label ]
  ;;

  (** bottom border*)
  let outline_bot attr w label =
    let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
    let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1 in
    let label =
      if label |> I.width > w - 2
      then I.empty
      else label |> I.hpad (w - (label |> I.width |> ( + ) 1)) 0
    in
    let c, d = chr 0x256f, chr 0x2570 in
    I.zcat [ label; I.hcat [ d; hbar; c ] ]
  ;;

  let make_label max_width label_str =
    I.strf " %s " (truncate_string (max_width - 2) label_str)
  ;;

  (** Internal function for rendering a border box with known dimensions and padding.*)
  let border_box_intern
    ?(border_attr = A.empty)
    ?(label_top = I.empty)
    ?(label_bottom = I.empty)
    ~w
    ~h
    ~pad
    ~pad_w
    ~pad_h
    input
    =
    (*can't go below 1 internal width or things get weird*)
    let h = if pad_h < 1 then Int.max h 1 else h in
    let w = if pad_w < 1 then Int.max w 1 else w in
    (* this is a weird quirk, but we have to be careful of runaway size expansion.
       If we increase the width of the space by making the vbar longer than the input ui element it will be able to expand to fill that space.
       That will then increase the vbar and increase the height etc etc untill the max height is reached*)
    let vbar =
      I.uchar border_attr (Uchar.of_int 0x2502) 1 (h + (pad_h * 2))
      |> Ui.atom
      |> Ui.resize ~h:0
    in
    Ui.vcat
      [ outline_top border_attr w label_top |> Ui.atom |> Ui.resize ~w:0
      ; Ui.hcat
          [ vbar
          ; I.void pad_w 1 |> Ui.atom
          ; Ui.vcat
              [ I.void 1 pad_h |> Ui.atom
              ; input |> Ui.resize ~pad
              ; I.void 1 pad_h |> Ui.atom
              ]
          ; I.void pad_w 1 |> Ui.atom
          ; vbar
          ]
      ; outline_bot border_attr w label_bottom |> Ui.atom |> Ui.resize ~w:0
      ]
  ;;
end

open Internal

let with_border_attr
  ?(pad = neutral_grav)
  ?(pad_w = 2)
  ?(pad_h = 1)
  ?label_top
  ?label_bottom
  get_border
  input
  =
  let size = Lwd.var (0, 0) in
  let layout_width = Lwd.var 0 in
  let input =
    let$ input = input in
    (*We need this later to determine the max with*)
    layout_width $= (input |> Ui.layout_width);
    input
    (*This lets us tell the input to be a flexible size*)
    |> Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then Lwd.set size (w, h))
  in
  (*This is original width and height of the input before padding or anything *)
  let$ o_w, o_h = Lwd.get size
  and$ input = input
  and$ border_attr = get_border in
  let w, h = o_w + (pad_w * 2), o_h in
  let h = h in
  let bbox =
    border_box_intern
      ~border_attr
      ?label_top:(label_top |> Option.map (make_label (w - 2)))
      ?label_bottom:(label_bottom |> Option.map (make_label (w - 2)))
        (* ~label_bottom:(if has_focus then I.strf "focused" else I.strf "unfocused") *)
      ~w
      ~h
      ~pad
      ~pad_w
      ~pad_h
      input
  in
  (*If we want the input to be shrinkable we make it expandable, set it's width to something small and then set a max width for the whole box*)
  bbox
;;

let focusable
  ?pad
  ?pad_w
  ?pad_h
  ?label_top
  ?label_bottom
  ?(border_attr = A.empty)
  ?(focus_attr = A.fg A.blue)
  ?(focus = Focus.make ())
  ?(on_key = fun _ -> `Unhandled)
  input
  =
  let input =
    input
    |> Lwd.map2 (focus |> Focus.status) ~f:(fun focus ui ->
      ui |> Ui.keyboard_area ~focus on_key)
  in
  with_border_attr
    ?pad
    ?pad_w
    ?pad_h
    ?label_top
    ?label_bottom
    (let$ focus = Focus.status focus in
     if Focus.has_focus focus then focus_attr else border_attr)
    input
;;

let box ?pad ?pad_w ?pad_h ?label_top ?label_bottom ?(border_attr = A.empty) input =
  with_border_attr
    ?pad
    ?pad_w
    ?pad_h
    ?label_top
    ?label_bottom
    (border_attr |> Lwd.pure)
    input
;;

let static
  ?(pad = neutral_grav)
  ?(pad_w = 2)
  ?(pad_h = 1)
  ?label_top
  ?label_bottom
  ?(border_attr = A.empty)
  ui
  =
  let Ui.{ w; h; _ } = Ui.layout_spec ui in
  Internal.border_box_intern
    ~w
    ~h
    ~pad
    ~pad_w
    ~pad_h
    ?label_top
    ?label_bottom
    ~border_attr
    ui
;;
