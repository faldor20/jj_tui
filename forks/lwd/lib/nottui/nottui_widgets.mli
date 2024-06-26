open Notty
open Nottui

val empty_lwd : ui Lwd.t

(* Primitive printing *)

(** Ui element from a string *)
val string : ?attr:attr -> string -> ui

(** Ui element from an int *)
val int : ?attr:attr -> int -> ui

(** Ui element from a boolean *)
val bool : ?attr:attr -> bool -> ui

(** Ui element from a float *)
val float_ : ?attr:attr -> float -> ui

(** Printf support *)
val printf : ?attr:attr -> ('a, unit, string, ui) format4 -> 'a
val kprintf : (ui -> 'a) -> ?attr:attr -> ('b, unit, string, 'a) format4 -> 'b

val fmt : ?attr:attr -> ('a, Format.formatter, unit, ui) format4 -> 'a
val kfmt : (ui -> 'a) -> ?attr:attr -> ('b, Format.formatter, unit, 'a) format4 -> 'b

(* window manager *)
type window_manager
val window_manager : ui Lwd.t -> window_manager
val window_manager_view : window_manager -> ui Lwd.t
val window_manager_overlays : window_manager -> ui Lwd.t Lwd_table.t

(* FIXME Menu *)
val menu_overlay : window_manager -> gravity -> ?dx:int -> ?dy:int -> ui Lwd.t -> ui -> ui
val main_menu_item : window_manager -> string -> (unit -> ui Lwd.t) -> ui Lwd.t
val sub_menu_item : window_manager -> string -> (unit -> ui Lwd.t) -> ui Lwd.t
val sub_entry : string -> (unit -> unit) -> ui

(* FIXME Explain how scrolling works *)
val scroll_step : int
type scroll_state = { position : int; bound : int; visible : int; total : int }
val default_scroll_state : scroll_state

val vscroll_area :
  state:scroll_state Lwd.t ->
  change:([> `Action | `Content ] -> scroll_state -> unit) ->
  ui Lwd.t -> ui Lwd.t

(** A scrollable area that supports *)
val scrollbox: ui Lwd.t -> ui Lwd.t

(** Vertical pane that can be dragged to be bigger or smaller *)
val v_pane : ui Lwd.t -> ui Lwd.t -> ui Lwd.t
(** horizontal pane that can be dragged to be bigger or smaller *)
val h_pane : ?splitter_color:(Notty.A.color)-> ui Lwd.t -> ui Lwd.t -> ui Lwd.t

(** An editable text field.
 Supports navigating with arrow keys  *)
val edit_field :
  ?focus:Focus.handle ->
  (string * int) Lwd.t ->
  on_change:(string * int -> unit) ->
  on_submit:(string * int -> unit) -> ui Lwd.t

(* FIXME Tabs *)

val tabs : (string * (unit -> ui Lwd.t)) list -> ui Lwd.t

(* FIXME Flex box *)

val flex_box : ?w:int Lwd.t -> ui Lwd.t list -> ui Lwd.t

(** Shows the summary when folded, calls [f()] to compute a sub-widget when clicked on. Useful for displaying deep trees. Mouse only *)
val unfoldable :
  ?folded_by_default:bool ->
  ui Lwd.t -> (unit -> ui Lwd.t) -> ui Lwd.t

(** Horizontally stacks Ui elements *)
val hbox : ui Lwd.t list -> ui Lwd.t
(** Horizontally stacks ui elements *)
val vbox : ui Lwd.t list -> ui Lwd.t
(** Stacks Ui elements infront of one another *)
val zbox : ui Lwd.t list -> ui Lwd.t

(* FIXME List *)
val vlist : ?bullet:string -> ui Lwd.t list -> ui Lwd.t

val vlist_with :
  ?bullet:string ->
  ?filter:('a -> bool) Lwd.t ->
  ('a -> ui Lwd.t) -> 'a list Lwd.t -> ui Lwd.t

(* FIXME This should probably go somewhere else *)
val iterate :int -> ('a -> 'a) -> 'a -> 'a

val grid :
  ?max_h:int -> ?max_w:int ->
  ?pad:gravity -> ?crop:gravity -> ?bg:attr ->
  ?h_space:int -> ?v_space:int ->
  ?headers:ui Lwd.t list ->
  ui Lwd.t list list -> ui Lwd.t

(** A clickable button that calls [f] when clicked, labelled with a string. *)
val button : ?attr:attr -> string -> (unit -> unit) -> ui

(** A mouse_based file selection widget that opens at the current path *)
val file_select :
  ?abs:bool ->
  ?filter:(String.t -> bool) ->
  on_select:(string -> unit) -> unit -> ui Lwd.t

(** A toggle button that invokes the callback when toggled*)
val toggle : ?init:bool -> string Lwd.t -> (bool -> unit) -> ui Lwd.t

(** A toggle button that changes the state of the Lwd.var when toggled*)
val toggle' : string Lwd.t -> bool Lwd.var -> ui Lwd.t
