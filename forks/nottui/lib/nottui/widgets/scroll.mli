(** A keyboard scroll area that only scrolls in the vertical direction *)
val v_area : Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t

(** A scroll area that allows keyboard scrolling in both x and y directions*)
val area : ?focus:Nottui_main.Focus.status -> Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t

(** A scroll area that allows keyboard scrolling in both x and y directions and has no limits.
    This might be useful if you have some very dynamic content and the usual scroll area doesn't know how big things are*)
val infinite_area : ?offset:int * int -> Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t
