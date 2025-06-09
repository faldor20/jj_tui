open Nottui_main
module MyMap : Map.S with type key = int

(**Selectable list item with a ui and some data *)
type 'a multi_selectable_item =
  { data : 'a
    (**info attached to each ui elment in the list,  used for filtering and on_select callback *)
  ; id : int
  ; ui : selected:bool -> hovered:bool -> Ui.t Lwd.t
  }

type 'a maybe_multi_selectable =
  | Selectable of 'a multi_selectable_item
  | Filler of Ui.t Lwd.t

(** multi_selectable exclusions *)
val multi_selection_list_exclusions
  :  ?focus:Nottui_main.Focus.handle
  -> ?reset_selections:'s Signal.t
  -> ?on_selection_change:(hovered:'a -> selected:'a list -> unit)
  -> custom_handler:
       (selected:'a MyMap.t
        -> selectable_items:(int * 'a multi_selectable_item) array
        -> Nottui_main.Ui.key
        -> Nottui_main.Ui.may_handle)
  -> 'a maybe_multi_selectable array Lwd.t
  -> Nottui_main.ui Lwd.t

(** Same as [selection_list_custom] except that it supports not all element in the list being selectable *)
val selection_list_exclusions
  :  ?focus:Nottui_main.Focus.handle
  -> ?on_selection_change:('a -> unit)
  -> custom_handler:
       ('a multi_selectable_item -> Nottui_main.Ui.key -> Nottui_main.Ui.may_handle)
  -> 'a maybe_multi_selectable array Lwd.t
  -> Nottui_main.ui Lwd.t

(**Makes a ui element selectable.

   Takes [ui] and returns a function that appends '>' to the start when given [true] and ' ' when false

   Used in conjuction with [selection_list_custom]*)
val selectable_item
  :  Nottui_main.ui
  -> selected:bool
  -> hovered:bool
  -> Nottui_main.ui Lwd.t

val selectable_item_lwd
  :  Nottui_main.ui Lwd.t
  -> selected:bool
  -> hovered:bool
  -> Nottui_main.ui Lwd.t

(** multi selection list that allows for custom handling of keyboard events.
    Scrolls when the selection reaches the lower third
    Only handles up and down keyboard events. Use [~custom_handler] to do handle confirming your selection and such *)
val multi_selection_list_custom
  :  ?focus:Nottui_main.Focus.handle
  -> ?reset_selections:'s Signal.t
  -> ?on_selection_change:(hovered:'a -> selected:'a list -> unit)
  -> custom_handler:
       (selected:'a MyMap.t
        -> selectable_items:(int * 'a multi_selectable_item) array
        -> Nottui_main.Ui.key
        -> Nottui_main.Ui.may_handle)
  -> 'a multi_selectable_item list Lwd.t
  -> Nottui_main.ui Lwd.t

(** Selection list that allows for custom handling of keyboard events.
    Scrolls when the selection reaches the lower third
    Only handles up and down keyboard events. Use [~custom_handler] to do handle confirming your selection and such *)
val selection_list_custom
  :  ?focus:Nottui_main.Focus.handle
  -> ?on_selection_change:('a -> unit)
  -> custom_handler:
       ('a multi_selectable_item -> Nottui_main.Ui.key -> Nottui_main.Ui.may_handle)
  -> 'a multi_selectable_item list Lwd.t
  -> Nottui_main.ui Lwd.t

(** A filterable selectable list.

    This version allows you to implement custom handlers for keys and only provides functionality for moving up and down the list.

    For basic usage you likely want {!filterable_selection_list} which provides `Enter` and `Esc` handlers *)
val filterable_selection_list_custom
  :  ?focus:Nottui_main.Focus.handle
  -> filter_predicate:(string -> 'a -> bool)
  -> custom_handler:
       ('a multi_selectable_item -> Nottui_main.Ui.key -> Nottui_main.Ui.may_handle)
  -> filter_text_var:string Lwd.var
  -> 'a multi_selectable_item list Lwd.t
  -> Nottui_main.ui Lwd.t

(** Filterable selection list

    Allows filtering and selecting items in a list.
    Also handles shifting the list so that the selection dosen't go out of view
    @param ~filter_predicate Function called to deterimine if an items should be included
    @param ~on_confirm Called when user presses enter
    @param ?on_esc Called when user presses esc
    @param list_items List of items to be displayed/selected/filtered *)
val filterable_selection_list
  :  ?pad_w:int
  -> ?pad_h:int
  -> ?focus:Nottui_main.Focus.handle
  -> filter_predicate:(string -> 'a -> bool)
  -> ?on_esc:('a -> unit)
  -> on_confirm:('a -> unit)
  -> 'a multi_selectable_item list Lwd.t
  -> Nottui_main.ui Lwd.t
