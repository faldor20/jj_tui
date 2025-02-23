open Notty
open Nottui_main
open Lwd_infix
open Shared

let thrd (_, _, x) = x

type 'a multi_selectable_item =
  { data : 'a
  ; id : int
  ; ui : selected:bool -> hovered:bool -> Ui.t Lwd.t
  }

type 'a maybe_multi_selectable =
  | Selectable of 'a multi_selectable_item
  | Filler of Ui.t Lwd.t

module MyMap = Map.Make (Int)
let singe_space= Shared.string " ";;

(** Get a map of all the selectable items*)
let get_selectable_items_map (items : 'a maybe_multi_selectable array Lwd.t) =
  let selectable_items =
    let$ items = items in
    (*Map of selectable items with their id as key*)
    items
    |> Array.fold_left
         (fun map item ->
           match item with
           | Selectable item -> MyMap.add item.id (item.id, item) map
           | Filler _ -> map)
         MyMap.empty
  in
  selectable_items
;;

let get_selectable_items (items : 'a maybe_multi_selectable array Lwd.t) =
  let selectable_items =
    let$ items = items in
    (*Array of selectable items and their idx in the original array*)
    (*This impl uses obj.magic and could cause address boundy errors if anything goes wrong BE CAREFUL*)

    (*make an unallocated array for our new content*)
    let selectable_items = Array.make (Array.length items) (Obj.magic ()) in
    let _, final_len =
      items
      |> Array.fold_left
           (fun (i, selectable_count) item ->
             match item with
             | Selectable item ->
               (*copy any seletable items to the new array*)
               Array.set selectable_items selectable_count (i, item);
               i + 1, selectable_count + 1
             | Filler _ -> i + 1, selectable_count)
           (0, 0)
    in
    Array.sub selectable_items 0 final_len
  in
  selectable_items
;;

let multi_selection_list_exclusions
  ?(focus = Focus.make ())
  ?reset_selections
  ?(on_selection_change = fun ~hovered ~selected -> ())
  ~custom_handler
  (items : 'a maybe_multi_selectable array Lwd.t)
  =
  (*
     The rough overview is:
     1. Make a new list that only contains our selectable items
     2. Render the items, making sure to tell the selected one to render as selected.
     3. Calculate how much we should scroll by.
     4. offset by the scroll amount, apply size sensors and output final ui
  *)
  let selected_items_var = Lwd.var MyMap.empty in
  (*provides a way to set this from the outside*)
  reset_selections
  |> Option.iter (Signal.sub (fun x -> selected_items_var $= MyMap.empty));
  (*Lets external functions to reset the selection*)
  (*hovered var is a tuple of (id, overall_idx,selection_idx)*)
  (*we set it up this way so we can avoid double rendering. We sometimes wish to change the value of the hover var during rendering and that would not update till the next  render and cause a re-render*)
  let hovered_var = ref (0, 0, 0) in
  let hover_changed = Lwd.var () in
  let selected_position = Lwd.var (0, 0) in
  let selectable_items_map = get_selectable_items_map items in
  let selectable_items = get_selectable_items items in
  (*handle selections*)
  let render_items =
    let$* focus = focus |> Focus.status
    and$ items, selectable_items =
      (* This doesn't depend on changes in focus but it should update whenever there are new items or a selection change*)
      let$ items = items
      and$ selectable_items = selectable_items in
      (*We are only beeking this one because we only want this run when the items list changes*)
      let hovered_id, hovered_ovearll_idx, hovered_selection_idx = !hovered_var in
      (*TODO: This is obviously very slow*)
      selected_items_var
      $= (Lwd.peek selected_items_var
          |> MyMap.filter (fun selected value ->
            selectable_items |> Array.exists (fun (_, item) -> item.id = selected)));
      (*first we handle upading our selection if the list of items has changed*)
      (* We do this here to ensure that the selected var is updated before we render to avoid double rendering*)
      let hovered_id, hovered_ovearll_idx, hovered_selection_idx =
        (*If the id is not in the list anymore then we should just pick the closest item by index*)
        (* We have a few progressively less ideal states here:
           1. We found our exact same id item in a new place
           2. We found an item that's in the same location as the previous one
           3. We just return something
        *)
        selectable_items
        |> Array.fold_left
             (fun (count, acc) (idx, item) ->
               let nCount = count + 1 in
               match acc with
               | `Found _ -> nCount, acc
               | `Same_idx _ ->
                 if item.id = hovered_id
                 then nCount, `Found (item.id, idx, count)
                 else nCount, acc
               | `Searching _ ->
                 if item.id = hovered_id
                 then nCount, `Found (item.id, idx, count)
                 else if count == hovered_selection_idx
                 then nCount, `Same_idx (item.id, idx, count)
                 else nCount, `Searching (item.id, idx, count))
             (0, `Searching (0, 0, 0))
        |> snd
        |> function
        | `Found (id, idx, count) -> id, idx, count
        | `Same_idx (id, idx, count) -> id, idx, count
        | `Searching (id, idx, count) -> id, idx, count
      in
      hovered_var := hovered_id, hovered_ovearll_idx, hovered_selection_idx;
      if Array.length selectable_items > 0
      then (
        let item_idx, item = selectable_items.(hovered_selection_idx) in
        on_selection_change
          ~hovered:item.data
          ~selected:
            (Lwd.peek selected_items_var |> MyMap.to_list |> List.map (fun (_, a) -> a));
        items, selectable_items)
      else items, selectable_items
    and$ _ = Lwd.get hover_changed
    and$ selected_items = Lwd.get selected_items_var in
    (* FIXME: can i just get rid of all the other parts of the hovered var now that we store the id?*)
    let hovered_id, _, _= !hovered_var in
    (*==== Rendering The list ====*)
    (* Ui.vcat can be a little weird when the *)
    if items |> Array.length = 0
    then Ui.empty |> Lwd.pure
    else
      items
      |> Array.mapi (fun i x ->
        match x with
        (*Becasue selectable has a space used for the selection pointer, filler also needs a space*)
        | Filler ui -> ui|>$(fun x-> Ui.hcat[ singe_space ; x])
        | Selectable x ->
          let hovered = hovered_id == x.id in
          let selected = selected_items |> MyMap.mem x.id in
          if hovered
          then
            x.ui ~hovered ~selected
            |>$ Ui.transient_sensor (fun ~x ~y ~w:_ ~h:_ () ->
              if (x, y) <> Lwd.peek selected_position then selected_position $= (x, y))
          else x.ui ~hovered ~selected)
      |> Array.to_list
      |> Shared.vbox
      |>$ Ui.keyboard_area ~focus (function
        | `Arrow `Up, [] ->
          let hovered_idx = max ((!hovered_var |> thrd) - 1) 0 in
          let hovered = (selectable_items.(hovered_idx) |> snd).id, 0, hovered_idx in
          hovered_var := hovered;
          Lwd.set hover_changed ();
          on_selection_change
            ~hovered:(selectable_items.(hovered_idx) |> snd).data
            ~selected:
              (Lwd.peek selected_items_var |> MyMap.to_list |> List.map (fun (_, a) -> a));
          `Handled
        | `Arrow `Down, [] ->
          let hovered_idx =
            Int.max
              (min ((!hovered_var |> thrd) + 1) ((selectable_items |> Array.length) - 1))
              0
          in
          let hovered = (selectable_items.(hovered_idx) |> snd).id, 0, hovered_idx in
          hovered_var := hovered;
          Lwd.set hover_changed ();
          on_selection_change
            ~hovered:(selectable_items.(hovered_idx) |> snd).data
            ~selected:
              (Lwd.peek selected_items_var |> MyMap.to_list |> List.map (fun (_, a) -> a));
          `Handled
        | `ASCII ' ', [] ->
          let hovered_id, _, hovered_idx = !hovered_var in
          let data = (selectable_items.(hovered_idx) |> snd).data in
          let selected = Lwd.peek selected_items_var in
          if selected |> MyMap.mem hovered_id
          then Lwd.set selected_items_var (MyMap.remove hovered_id selected)
          else Lwd.set selected_items_var (MyMap.add hovered_id data selected);
          (* TODO: make sure this actually apllies, there is some chance the peek will no update*)
          on_selection_change
            ~hovered:data
            ~selected:
              (Lwd.peek selected_items_var |> MyMap.to_list |> List.map (fun (_, a) -> a));
          `Handled
        | `Escape, [] ->
          Lwd.set selected_items_var MyMap.empty;
          `Handled
        | a -> custom_handler ~selected:(Lwd.peek selected_items_var) ~selectable_items a)
  in
  let rendered_size_var = Lwd.var (0, 0) in
  (*Handle scrolling*)
  let scrollitems =
    let size_var = Lwd.var (0, 0) in
    let shift_amount =
      (*get the actual idx not just the selection number*)
      let$ selected_idx =
        Lwd.map2 (Lwd.get hover_changed) selectable_items ~f:(fun () selectable ->
          let _, _, hovered_idx = !hovered_var in
          if Array.length selectable > hovered_idx
          then selectable.(hovered_idx) |> fst
          else 0)
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

let selection_list_exclusions
  ?(focus = Focus.make ())
  ?(on_selection_change = fun _ -> ())
  ~custom_handler
  (items : 'a maybe_multi_selectable array Lwd.t)
  =
  (*
     The rough overview is:
     1. Make a new list that only contains our selectable items
     2. Render the items, making sure to tell the selected one to render as selected.
     3. Calculate how much we should scroll by.
     4. offset by the scroll amount, apply size sensors and output final ui
  *)
  let selected_var = Lwd.var 0 in
  let selected_position = Lwd.var (0, 0) in
  let selectable_items = get_selectable_items items in
  (*handle selections*)
  let render_items =
    let$* focus = focus |> Focus.status
    and$ items, hovered, selectable_items =
      (* This doesn't depend on changes in focus but it should update whenever there are new items or a selection change*)
      let$ items = items
      and$ selectable_items = selectable_items
      and$ selected = Lwd.get selected_var in
      (* First ensure if our list has gotten shorter we haven't selected off the list*)
      (* We do this here to ensure that the selected var is updated before we render to avoid double rendering*)
      let max_selected = Int.max 0 (Array.length selectable_items - 1) in
      let selected =
        let selected = Lwd.peek selected_var in
        if Int.min selected max_selected <> selected
        then (
          selected_var $= max_selected;
          max_selected)
        else selected
      in
      if Array.length selectable_items > 0
      then (
        let item_idx, item = selectable_items.(selected) in
        on_selection_change item.data;
        items, item_idx, selectable_items)
      else items, 0, selectable_items
    in
    (* Ui.vcat can be a little weird when the *)
    if items |> Array.length = 0
    then Ui.empty |> Lwd.pure
    else
      items
      |> Array.mapi (fun i x ->
        match x with
        (*Becasue selectable has a space used for the selection pointer, filler also needs a space*)
        | Filler ui ->  ui|>$(fun x-> Ui.hcat[ singe_space ; x])
        | Selectable x ->
          if hovered == i
          then
            x.ui ~hovered:true ~selected:false
            |>$ Ui.transient_sensor (fun ~x ~y ~w:_ ~h:_ () ->
              if (x, y) <> Lwd.peek selected_position then selected_position $= (x, y))
          else x.ui ~hovered:false ~selected:false)
      |> Array.to_list
      |> Shared.vbox
      |>$ Ui.keyboard_area ~focus (function
        | `Arrow `Up, [] ->
          let selected = max (Lwd.peek selected_var - 1) 0 in
          selected_var $= selected;
          `Handled
        | `Arrow `Down, [] ->
          let selected =
            Int.max
              (min (Lwd.peek selected_var + 1) ((selectable_items |> Array.length) - 1))
              0
          in
          selected_var $= selected;
          `Handled
        | a -> custom_handler (selectable_items.(Lwd.peek selected_var) |> snd) a)
  in
  let rendered_size_var = Lwd.var (0, 0) in
  (*Handle scrolling*)
  let scrollitems =
    let size_var = Lwd.var (0, 0) in
    let shift_amount =
      (*get the actual idx not just the selection number*)
      let$ selected_idx =
        Lwd.map2 (Lwd.get selected_var) selectable_items ~f:(fun selected selectable ->
          if Array.length selectable > selected then selectable.(selected) |> fst else 0)
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

let selectable_item ui ~selected ~hovered =
  let height = Ui.layout_height ui in
  let prefix =
    if selected && hovered (* (Uchar.of_int 0x2265) *)
    then I.uchar A.(bg A.cyan ++ fg black ++ st bold) (Uchar.of_char 'x') 1 height
    else if selected
    then I.uchar A.(bg A.cyan ++ fg black ++ st bold) (Uchar.of_char 'o') 1 height
    else if hovered
    then I.uchar A.(fg A.cyan ++ st bold) (Uchar.of_int 0x25b6) 1 height
    else I.char A.empty ' ' 1 height
  in
  Ui.hcat [ prefix |> Ui.atom; ui ] |> Lwd.pure
;;

let selectable_item_lwd ui ~selected ~hovered =
  let$ ui = ui in
  let height = Ui.layout_height ui in
  let prefix =
    if selected && hovered
    then I.uchar A.(bg blue) (Uchar.of_int 0x2265) 1 height
    else if selected
    then I.char A.(bg blue) '=' 1 height
    else if hovered
    then I.char A.(bg blue) '>' 1 height
    else I.char A.empty ' ' 1 height
  in
  Ui.hcat [ prefix |> Ui.atom; ui ]
;;

let multi_selection_list_custom
  ?(focus = Focus.make ())
  ?reset_selections
  ?(on_selection_change = fun ~hovered ~selected -> ())
  ~custom_handler
  (items : 'a multi_selectable_item list Lwd.t)
  =
  multi_selection_list_exclusions
    ~focus
    ?reset_selections
    ~on_selection_change
    ~custom_handler
    (items
     |>$ fun items ->
     let selectable_items = Array.make (List.length items) (Obj.magic ()) in
     items |> List.iteri (fun i x -> Array.set selectable_items i (Selectable x));
     selectable_items)
;;

let selection_list_custom
  ?(focus = Focus.make ())
  ?(on_selection_change = fun _ -> ())
  ~custom_handler
  (items : 'a multi_selectable_item list Lwd.t)
  =
  selection_list_exclusions
    ~focus
    ~on_selection_change
    ~custom_handler
    (items
     |>$ fun items ->
     let selectable_items = Array.make (List.length items) (Obj.magic ()) in
     items |> List.iteri (fun i x -> Array.set selectable_items i (Selectable x));
     selectable_items)
;;

let filterable_selection_list_custom
  ?(focus = Focus.make ())
  ~(filter_predicate : string -> 'a -> bool)
  ~custom_handler
  ~filter_text_var
  (items : 'a multi_selectable_item list Lwd.t)
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
      ~custom_handler:(fun item x ->
        (*TODO is this needed? won't the items provided already be filtered anyway?*)
        custom_handler item x)
      items
  in
  items
;;

let filterable_selection_list
  ?(pad_w = 1)
  ?(pad_h = 0)
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
         ~custom_handler:(fun item key ->
           match key with
           | `Enter, [] ->
             item.data |> on_confirm;
             `Handled
           | `Escape, [] ->
             item.data |> on_esc;
             `Handled
           | _ -> `Unhandled)
  in
  let max_width = Lwd.var 5 in
  vbox
    [ filter_text_ui |> Border_box.box ~pad_w ~pad_h
    ; (list_ui
       |> Border_box.box ~pad_w ~pad_h
       |>$ fun x ->
       let mw = (x |> Ui.layout_spec).mw in
       if mw > Lwd.peek max_width then max_width $= mw;
       x)
    ]
  |> Lwd.map2 (Lwd.get max_width) ~f:(fun mw ui -> ui |> Ui.resize ~mw)
;;
