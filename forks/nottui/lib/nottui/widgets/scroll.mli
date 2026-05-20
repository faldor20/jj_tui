(** A keyboard scroll area that only scrolls in the vertical direction.

    [show_scrollbars] defaults to [true] and controls displaying the visual scroll indicator.*)
val v_area
  :  ?reset_on_content_change:bool
  -> ?show_scrollbars:bool
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(** A scroll area that allows keyboard scrolling in both x and y directions.

    [show_scrollbars] defaults to [true] and controls displaying the visual scroll indicator.*)
val area
  :  ?reset_on_content_change:bool
  -> ?show_scrollbars:bool
  -> ?focus:Nottui_main.Focus.status
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(** A scroll area that allows keyboard scrolling in both x and y directions and has no limits.
    This might be useful if you have some very dynamic content and the usual scroll area doesn't know how big things are*)
val infinite_area : ?offset:int * int -> Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t
