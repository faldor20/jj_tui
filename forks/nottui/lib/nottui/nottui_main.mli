open Notty

(** Nottui augments Notty with primitives for laying out user interfaces (in the
    terminal) and reacting to input events. *)

(** {1 Focus (defining and managing active objects)} *)

module Focus : sig
  (** A [handle] represents a primitive area that can request, receive and lose
      the focus. A visible UI is made of many handles, of which at most one can
      be active. *)
  type handle

  (** Create a new handle *)
  val make : unit -> handle

  (** Request the focus *)
  val request : handle -> unit

  (** Release the focus (if the handle has it) *)
  val release : handle -> unit

  (** [status] represents the state in which a handle can be.
      Externally we care about having or not the focus, which can be queried
      with the [has_focus] function. Internally, [status] also keeps track of
      conflicts (if multiple handles [request]ed the focus). *)
  type status

  (** A status that has no focus and no conflicts *)
  val empty : status

  (** Get the status of a focus [handle]. The [status] is a reactive value:
      it will evolve over time, as focus is received or lost. *)
  val status : handle -> status Lwd.t

  (** Check if this [status] corresponds to an active focus *)
  val has_focus : status -> bool

  (** TODO
      This implements a more general concept of "reactive auction":

      - multiple parties are competing for a single resource (focus here, but
        for instance a tab component can only display a single tab among many).

      - the result can evolve over time, parties can join or leave, or bid
        "more". *)

  (** Request the focus and add to the focus stack. 
  WARNING: The focus stack is global, if you render multiple nottui ui's you may not want to use this *)
  val request_reversable : handle -> unit

  (** Release the focus (if the handle has it) and restore the last focus on the stack *)
  val release_reversable : handle -> unit
end

(** {1 Gravity (horizontal and vertical alignments)} *)

module Gravity : sig
  (** A gravity is a pair of directions  along the horizontal and vertical
      axis.

      Horizontal axis goes from left to right and vertical axis from top to
      bottom.

      [`Negative] direction means left / top bounds, [`Neutral] means center
      and [`Positive] means right / bottom. *)
  type direction =
    [ `Negative
    | `Neutral
    | `Positive
    ]

  (** Printing directions *)
  val pp_direction : Format.formatter -> direction -> unit

  (** The gravity type is a pair of an horizontal and a vertical gravity *)
  type t

  (** Printing gravities *)
  val pp : Format.formatter -> t -> unit

  (** Make a gravity value from an [h]orizontal and a [v]ertical directions. *)
  val make : h:direction -> v:direction -> t

  (** Default (negative, aligning to the top-left) gravity. *)
  val default : t

  (** Get the horizontal direction *)
  val h : t -> direction

  (** Get the vertical direction *)
  val v : t -> direction
end

type gravity = Gravity.t

(** {1 Primitive combinators for making user interfaces} *)

module Ui : sig
  type t
  (* Type of UI elements *)

  (** Printing UI element *)
  val pp : Format.formatter -> t -> unit

  (** {1 Layout specifications} *)

  (** The type of layout specifications.

      For each axis, layout is specified as a pair of integers:
      - a fixed part that is expressed as a number of columns or rows
      - a stretchable part that represents a strength used to share the
        remaining space (or 0 if the UI doesn't extend over free space) *)
  type layout_spec =
    { w : int
    ; h : int
    ; sw : int
    ; sh : int
    ; mw : int
    ; mh : int
    }

  (** Printing layout specification *)
  val pp_layout_spec : Format.formatter -> layout_spec -> unit

  (** Get the layout spec for an UI element *)
  val layout_spec : t -> layout_spec

  (** Get the layout width component of an UI element *)
  val layout_width : t -> int

  (** Get the layout stretch width strength of an UI element *)
  val layout_stretch_width : t -> int

  (** Get the layout height component of an UI element *)
  val layout_height : t -> int

  (** Get the layout height strength of an UI element *)
  val layout_stretch_height : t -> int

  (** Get the layout max_width of an UI element *)
  val layout_max_width : t -> int

  (** Get the layout max_height of an UI element *)
  val layout_max_height : t -> int

  (** {1 Primitive images} *)

  (** The empty surface: it occupies no space and does not do anything *)
  val empty : t

  (** Primitive surface that displays a Notty image *)
  val atom : image -> t

  (** Void space of dimensions [x,y]. Useful for padding and interstitial
      space. *)
  val space : int -> int -> t

  (** {1 Event handles} *)

  (** The type of handlers for mouse events. They receive the (absolute)
      coordinates of the mouse, the button that was clicked.

      In return they indicate whether the event was handled or if the mouse is
      "grabbed".

      When grabbed, two functions [on_move] and [on_release] should be
      provided. The [on_move] function will be called when the mouse move while
      the button is pressed and the [on_release] function is called when the
      button is released.

      During that time, no other mouse input events can be dispatched. *)
  type mouse_handler =
    x:int
    -> y:int
    -> Unescape.button
    -> [ `Handled
       | `Unhandled
       | `Grab of (x:int -> y:int -> unit) * (x:int -> y:int -> unit)
       ]

  (** Key handlers normally reacts to keyboard input but a few special keys are
      defined to represent higher-level actions.
      Copy and paste, as well as focus movements. *)
  type semantic_key =
    [ (* Clipboard *)
      `Copy
    | `Paste
    | (* Focus management *)
      `Focus of
      [ `Out | `Next | `Prev | `Left | `Right | `Up | `Down ]
    ]

  (** A key is the pair of a main key and a list of modifiers *)
  type key =
    [ Unescape.special | `Uchar of Uchar.t | `ASCII of char | semantic_key ]
    * Unescape.mods

  (** An event is propagated until it gets handled.
      Handler functions return a value of type [may_handle] to indicate
      whether the event was handled, not handled, or should be remapped to aonother event. *)
  type may_handle =
    [ `Unhandled
    | `Handled
    | `Remap of key
    ]

  (** Specification of mouse inputs, taken from Notty *)
  type mouse = Unescape.mouse

  type event =
    [ `Key of key
    | `Mouse of mouse
    | `Paste of Unescape.paste
    ]
  (* The type of input events. *)

  (** Handle mouse events that happens over an ui. *)
  val mouse_area : mouse_handler -> t -> t

  (** Define a focus receiver, handle keyboard events over the focused area. Distinct from [event_filter] because [`Focus *] events will move focus between these areas *)
  val keyboard_area : ?focus:Focus.status -> (key -> may_handle) -> t -> t

  (** Check if this UI has focus, either directly (it is a focused
      [keyboard_area]), or inherited (one of the child is a focused
      [keyboard_area]). *)
  val has_focus : t -> bool

  (** A hook that intercepts and can interrupt events when they reach a
      sub-part of the UI. *)
  val event_filter
    :  ?focus:Focus.status
    -> ([ `Key of key | `Mouse of mouse ] -> may_handle)
    -> t
    -> t

  (** {1 Sensors}

      Sensors are used to observe the physical dimensions after layout has been
      resolved. *)

  (** The size sensor callback tells you the [w]idth and [h]eight of UI.
      The sensor is invoked only when the UI is visible. *)
  type size_sensor = w:int -> h:int -> unit

  (** Attach a size sensor to an image *)
  val size_sensor : size_sensor -> t -> t

  (** The frame sensor callback gives you the whole rectangle where the widget
      is displayed.

      The first for components are applied during before visiting children,
      the last unit is applied after visiting children. *)
  type frame_sensor = x:int -> y:int -> w:int -> h:int -> unit -> unit

  (** Attach a transient frame sensor: the callback will be invoked only once,
      on next frame. *)
  val transient_sensor : frame_sensor -> t -> t

  (** Attach a permanent sensor: the callback will be invoked on every frame.
      Note that this can have a significant impact on performance. *)
  val permanent_sensor : frame_sensor -> t -> t

  (** {1 Composite images} *)

  (** Override the layout specification of the ui with provided [w](width), [h](height),
      [sw](stretch width) or [sh](stretch height)
      [mw](max width) or [mh](max height).

      [pad] and [crop] are used to determine how to align the UI when there is
      too much or not enough space.

      [bg] is used to fill the padded background. *)
  val resize
    :  ?w:int
    -> ?h:int
    -> ?sw:int
    -> ?sh:int
    -> ?mw:int
    -> ?mh:int
    -> ?pad:Gravity.t
    -> ?crop:Gravity.t
    -> ?bg:attr
    -> t
    -> t

  val resize_to : layout_spec -> ?pad:Gravity.t -> ?crop:Gravity.t -> ?bg:attr -> t -> t

  (** Shift the contents of a UI by a certain amount.
      Positive values crop the image while negative values pad.

      This primitive is used to implement scrolling. *)
  val shift_area : int -> int -> t -> t

  (** Horizontally join two images *)
  val join_x : t -> t -> t

  (** Vertically join two images *)
  val join_y : t -> t -> t

  (** Superpose two images. The right one will be on top. *)
  val join_z : t -> t -> t

  (** Horizontal concatenation monoid *)
  val pack_x : t Lwd_utils.monoid

  (** Vertical concatenation monoid *)
  val pack_y : t Lwd_utils.monoid

  (** Superposition monoid *)
  val pack_z : t Lwd_utils.monoid

  (** Short-hand for horizontally joining a list of images *)
  val hcat : t list -> t

  (** Short-hand for vertically joining a list of images *)
  val vcat : t list -> t

  (** Short-hand for superposing a list of images *)
  val zcat : t list -> t
end

type ui = Ui.t

(** {1 Rendering user interfaces and dispatching input events} *)

module Renderer : sig
  (** The type of a renderer *)
  type t

  (** Size of a rendering surface, as a pair of width and height *)
  type size = int * int

  (** Create a new renderer.

      It maintains state to update output image and to dispatch events. *)
  val make : unit -> t

  (** Update the contents to be rendered to the given UI at a specific size *)
  val update : t -> size -> Ui.t -> unit

  (** Get the size of the last update *)
  val size : t -> size

  (** Render and return actual image *)
  val image : t -> image

  (** Dispatch a mouse event *)
  val dispatch_mouse : t -> Ui.mouse -> Ui.may_handle

  (** Dispatch a keyboard event *)
  val dispatch_key : t -> Ui.key -> Ui.may_handle

  (** Dispatch an event *)
  val dispatch_event : t -> Ui.event -> Ui.may_handle
end

(** {1 Main loop}

    Outputting an interface to a TTY and interacting with it *)

module Ui_loop : sig
  open Notty_unix

  (** Run one step of the main loop.

      Update output image describe by the provided [root].
      If [process_event], wait up to [timeout] seconds for an input event, then
      consume and dispatch it. *)
  val step
    :  ?process_event:bool
    -> ?timeout:float
    -> renderer:Renderer.t
    -> Term.t
    -> ui Lwd.root
    -> unit

  (** Repeatedly run steps of the main loop, until either:
      - [quit] becomes true,
      - the ui computation raises an exception,
      - if [quit_on_ctrl_q] was true or not provided, wait for Ctrl-Q event
      - if [quit_on_escape] was true or not provided, wait for Escape event

      Specific [term] or [renderer] instances can be provided, otherwise new
      ones will be allocated and released.

      To simulate concurrency in a polling fashion, tick function and period
      can be provided. Use the [Lwt] backend for real concurrency. *)
  val run
    :  ?tick_period:float
    -> ?tick:(unit -> unit)
    -> ?term:Term.t
    -> ?renderer:Renderer.t
    -> ?quit:bool Lwd.var
    -> ?quit_on_escape:bool
    -> ?quit_on_ctrl_q:bool
    -> ui Lwd.t
    -> unit
end
