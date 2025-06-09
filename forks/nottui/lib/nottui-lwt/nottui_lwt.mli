open Notty
open Nottui

(** Nottui_lwt exposes an Lwt-driven mainloop.

    UI execution is done in an Lwt thread and UI events can spawn and
    synchronize threads.
*)

(** FIXME: Refactor to use [Nottui.Ui.event]? *)
type event =
  [ `Key of Unescape.key
  | `Mouse of Unescape.mouse
  | `Paste of Unescape.paste
  | `Resize of int * int
  ]

(** Turn a stream of events into a stream of images. *)
val render
  :  ?quit:unit Lwt.t
  -> size:int * int
  -> event Lwt_stream.t
  -> ui Lwd.t
  -> image Lwt_stream.t

(** Run mainloop in [Lwt], until the [quit] promise is fulfilled.

    The ui is a normal [Lwd.t] value, but events are free to spawn asynchronous
    [Lwt] threads.
*)
val run : ?quit:(*?term:Term.t ->*) unit Lwt.t -> ui Lwd.t -> unit Lwt.t
