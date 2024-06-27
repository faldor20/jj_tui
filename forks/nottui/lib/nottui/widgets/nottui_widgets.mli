open Notty
open Nottui_main
include module type of Shared
val empty_lwd : ui Lwd.t


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


(** Shows the summary when folded, calls [f()] to compute a sub-widget when clicked on. Useful for displaying deep trees. Mouse only *)
val unfoldable :
  ?folded_by_default:bool ->
  ui Lwd.t -> (unit -> ui Lwd.t) -> ui Lwd.t


val grid :
  ?max_h:int -> ?max_w:int ->
  ?pad:gravity -> ?crop:gravity -> ?bg:attr ->
  ?h_space:int -> ?v_space:int ->
  ?headers:ui Lwd.t list ->
  ui Lwd.t list list -> ui Lwd.t

(** A clickable button that calls [f] when clicked, labelled with a string. *)
val button : ?attr:attr -> string -> (unit -> unit) -> ui


(** A toggle button that invokes the callback when toggled*)
val toggle : ?init:bool -> string Lwd.t -> (bool -> unit) -> ui Lwd.t

(** A toggle button that changes the state of the Lwd.var when toggled*)
val toggle' : string Lwd.t -> bool Lwd.var -> ui Lwd.t
