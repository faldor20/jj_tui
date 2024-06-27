open Notty
open Nottui_main

let neutral_grav = Gravity.make ~h:`Neutral ~v:`Neutral
let make_even num = num + (num mod 2 * 1)

let empty_lwd = Lwd.return Ui.empty
let mini, maxi, clampi = Lwd_utils.(mini, maxi, clampi)

let attr_clickable = A.(bg lightblue)

(** This is for shifting something away from the edge it is pushed against *)
let pad_edge x_pad y_pad grav ui =
  let y_pad =
    match grav |> Gravity.v with
    | `Negative ->
      -y_pad
    | `Neutral ->
      0
    | `Positive ->
      y_pad
  in
  match grav |> Gravity.h with
  | `Negative ->
    ui |> Ui.shift_area (-x_pad) y_pad
  | `Neutral ->
    ui
  | `Positive ->
    ui |> Ui.shift_area x_pad y_pad
;;

(** Ui element from a string *)
let string ?(attr = A.empty) str =
  let control_character_index str i =
    let len = String.length str in
    let i = ref i in
    while
      let i = !i in
      i < len && str.[i] >= ' '
    do
      incr i
    done;
    if !i = len then raise Not_found;
    !i
  in
  let rec split str i =
    match control_character_index str i with
    | j ->
      let img = I.string attr (String.sub str i (j - i)) in
      img :: split str (j + 1)
    | exception Not_found ->
      [ I.string attr (if i = 0 then str else String.sub str i (String.length str - i)) ]
  in
  Ui.atom (I.vcat (split str 0))
;;

(** Ui element from an int *)
let int ?attr x = string ?attr (string_of_int x)

(** Ui element from a boolean *)
let bool ?attr x = string ?attr (string_of_bool x)

(** Ui element from a float *)
let float_ ?attr x = string ?attr (string_of_float x)

(** Printf support *)
let printf ?attr fmt = Printf.ksprintf (string ?attr) fmt

(** asprintf support *)
let fmt ?attr fmt = Format.kasprintf (string ?attr) fmt

(** Printf support *)
let kprintf k ?attr fmt = Printf.ksprintf (fun str -> k (string ?attr str)) fmt

(** Printf support *)
let kfmt k ?attr fmt = Format.kasprintf (fun str -> k (string ?attr str)) fmt

(**A size sensor that automatically updates the size variable*)
let simpleSizeSensor ~size_var ui =
  ui
  |> Ui.size_sensor (fun ~w ~h ->
    if Lwd.peek size_var <> (w, h) then Lwd.set size_var (w, h))
;;

(** A simple un_navigateable input field that only allows typing and deleting content. Try using edit_field for something that allows navigating within the text*)
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

(** Horizontally stacks Ui elements *)
let hbox l = Lwd_utils.pack Ui.pack_x l

(** Horizontally stacks ui elements *)
let vbox l = Lwd_utils.pack Ui.pack_y l

(** Stacks Ui elements infront of one another *)
let zbox l = Lwd_utils.pack Ui.pack_z l


(** Horizontal/vertical box. We fill lines until there is no room,
    and then go to the next ligne. All widgets in a line are considered to
    have the same height.
    @param width dynamic width  (default 80) *)
let flex_box ?(w = Lwd.return 80) (l : Ui.t Lwd.t list) : Ui.t Lwd.t =
  let open Lwd.Infix in
  Lwd_utils.flatten_l l
  >>= fun l ->
  w
  >|= fun w_limit ->
  let rec box_render (acc : Ui.t) (i : int) l : Ui.t =
    match l with
    | [] -> acc
    | ui0 :: tl ->
      let w0 = (Ui.layout_spec ui0).Ui.w in
      if i + w0 >= w_limit
      then (* newline starting with ui0 *)
        Ui.join_y acc (box_render ui0 w0 tl)
      else (* same line *)
        box_render (Ui.join_x acc ui0) (i + w0) tl
  in
  box_render Ui.empty 0 l
;;

module List = struct 
  include List

(** intersperse elements of the list with items *)
let intersperse t ~sep =
  match t with
  | [] -> []
  | x :: xs -> x :: fold_right (fun y acc -> sep :: y :: acc) xs []
;;
end

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

