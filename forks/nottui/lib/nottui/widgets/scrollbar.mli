(** Scrollbar rendering primitives shared by scrollable widgets.

    This module draws small terminal-native scrollbar indicators that reflect a
    scroll position, the remaining overflow in each direction, and the current
    global thumb style configured through [Nottui_main.Ui.global_config].

    The intent is to keep scrollbar rendering logic in one place so higher-level
    scroll containers only need to provide viewport measurements and compose the
    resulting indicators around their content.

    Example: render a standalone vertical indicator beside some existing UI.

    {[ let indicator =
         Scrollbar.render
           `Vertical
           ~focused:true
           ~state:{ Scrollbar.position = 12; bound = 40 }
           ~length:8
        in
        Ui.join_x content indicator ]}

    Example: render a horizontal indicator below a viewport.

    {[ let bottom_bar =
         Scrollbar.render
           `Horizontal
           ~focused:false
           ~state:{ Scrollbar.position = 3; bound = 10 }
           ~length:20
        in
        Ui.join_y content bottom_bar ]}

    You may wish to shrink the length region of the bottom bar if you include vertical and horizontal scroll bars.
  having the horizontal `length` be -2 compared to the actual area of the length content makes the bars visually stop the same distance apart eg:
           │              │
  Normal:  ▼   Adjusted:  ▼ 
        ──▶          ──▶
    *)



(** Scroll position plus the maximum scroll bound for one axis. *)
type state =
  { position : int
  ; bound : int
  }

(** Scrollbar axis selector. *)
type axis = [ `Horizontal | `Vertical ]

(** [render axis ~focused ~state ~length] produces a scrollbar indicator for
    one axis.

    - [focused] brightens the thumb and directional markers
    - [state.position] is the current scroll offset
    - [state.bound] is the maximum reachable offset
    - [length] is the viewport size along the given axis

    If [visible <= 0], the result is [Ui.empty]. *)
val render : axis -> focused:bool -> state:state -> visible:int -> Nottui_main.ui
