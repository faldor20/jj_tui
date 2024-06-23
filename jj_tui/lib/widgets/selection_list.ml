open Notty
open Nottui
open Lwd_infix
open! Util
open Shared
open Border_box

(**Selectable list item with a ui and some data *)
type 'a selectable_item = {
    data : 'a
  (**info attached to each ui elment in the list,  used for filtering and on_select callback *)
  ; ui : bool -> Ui.t
}

(**Makes a ui element selectable.

   Takes [ui] and returns a function that appends '>' to the start when given [true] and ' ' when false

   Used in conjuction with [selection_list_custom]*)
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
    (* Ui.vcat can be a little weird when the *)
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

type 'a maybeSelectable =
  | Selectable of 'a selectable_item
  | Filler of Ui.t

(** Same as [selection_list_custom] except that it supports not all element in the list being selectable *)
let selection_list_exclusions
  ?(focus = Focus.make ())
  ?(on_selection_change = fun _ -> ())
  ~custom_handler
  (items : 'a maybeSelectable array Lwd.t)
  =
  (*
     The rough overview is:
     1. Make a lookup list that has the indexes of all the selectable items within the overall list, we will be selecting from those
     2. Render the items, making sure to tell the selected one to render as selected. 
     3. Calculate how much we should scroll by. 
     4. offset by the scroll amount, apply size sensors and output final ui
  *)
  let selected_var = Lwd.var 0 in
  let selected_position = Lwd.var (0, 0) in
  let selectable_item_indexes =
    let$ items = items in
    let lut = Array.make (Array.length items) 0 in
    let final_len =
      items
      |> Base.Array.foldi ~init:0 ~f:(fun i selectable_count item ->
        match item with
        | Selectable _ ->
          Array.set lut selectable_count i;
          selectable_count + 1
        | Filler _ ->
          selectable_count)
    in
    Array.sub lut 0 final_len
  in
  (*handle selections*)
  let render_items =
    let$ focus = focus |> Focus.status
    and$ items, selected, selectable_item_indexes =
      (* This doesn't depend on changes in focus but it should update whenever there are new items or a selection change*)
      let$ items = items
      and$ selectable_item_indexes = selectable_item_indexes
      and$ selected = Lwd.get selected_var in
      (* First ensure if our list has gotten shorter we haven't selected off the list*)
      (* We do this here to ensure that the selected var is updated before we render to avoid double rendering*)
      let max_selected = Int.max 0 (Array.length selectable_item_indexes - 1) in
      if Int.min selected max_selected <> selected then selected_var $= max_selected;
      let selected = Lwd.peek selected_var in
      (* We lookup the index of the selected item in the list of items, remeber our list isn't entirely selectable items*)
      if Array.length selectable_item_indexes > 0
      then (
        let item_idx = selectable_item_indexes.(selected) in
        items.(item_idx) |> fun x ->
        (match x with
         | Selectable s ->
           on_selection_change s.data
         | Filler _ ->
           failwith "selected an item that wasn't selectable. This shouldn't be possible");
        items, item_idx, selectable_item_indexes)
      else items, 0, selectable_item_indexes
    in
    (* Ui.vcat can be a little weird when the *)
    items
    |> Array.mapi (fun i x ->
      match x with
      | Filler ui ->
        ui
      | Selectable x ->
        if selected == i
        then
          x.ui true
          |> Ui.transient_sensor (fun ~x ~y ~w:_ ~h:_ () ->
            if (x, y) <> Lwd.peek selected_position then selected_position $= (x, y))
        else x.ui false)
    |> Array.to_list
    |> Ui.vcat
    |> Ui.keyboard_area ~focus (function
      | `Arrow `Up, [] ->
        let selected = max (Lwd.peek selected_var - 1) 0 in
        selected_var $= selected;
        `Handled
      | `Arrow `Down, [] ->
        let selected =
          Int.max
            (min
               (Lwd.peek selected_var + 1)
               ((selectable_item_indexes |> Array.length) - 1))
            0
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
      (*get the actual idx not just the selection number*)
      let$ selected_idx =
        Lwd.map2
          (Lwd.get selected_var)
          selectable_item_indexes
          ~f:(fun selected indexes ->
            if Array.length indexes > 0 then indexes.(selected) else 0)
      and$ size = Lwd.get size_var
      and$ length = items |>$ Array.length
      and$ ren_size = Lwd.get rendered_size_var in
      (*portion of the total size of the element that is rendered*)
      let size_ratio =
        (ren_size |> snd |> float_of_int) /. (size |> snd |> float_of_int)
      in
      (*Tries to ensure that we start scrolling the list when we've selected about a third of the way down (using 3.0 causes weird jumping, so i use just less than )*)
      let offset = size_ratio *. ((length |> float_of_int) /. 2.9) in
      (*portion of the list that is behind the selection*)
      let list_ratio =
        ((selected_idx |> float_of_int) +. offset) /. (length |> float_of_int)
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
