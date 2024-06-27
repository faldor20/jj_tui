(** A border box that allows setting the border style from an [Lwd.t] prefer [Border_box.focusable] or [Border_box.box] unless you need this *)
val with_border_attr
  :  ?pad:Nottui_main.gravity
  -> ?pad_w:int
  -> ?pad_h:int
  -> ?label_top:string
  -> ?label_bottom:string
  -> Notty.attr Lwd.t
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(** Creates a bordered box around the given [input] widget. This box will change colour when focused

    @param scaling
      Controls how the input widget is sized within the border box. Can be:
      - [`Static] - The input widget is not resized.
      - [`Expand sw] - The input widget is allowed to expand to fill the available space, with a stretch width [sw].
      - [`Shrinkable (min_width, sw)] - The input widget is allowed to shrink to a minimum width of [min_width], and expand with a stretch width [sw].
    @param pad The padding around the input widget within the border box.
    @param pad_w The horizontal padding around the input widget.
    @param pad_h The vertical padding around the input widget.
    @param label An optional label to display within the border box.
    @param input The input widget to be bordered.
    @param border_attr Style for the border, defaults to [A.empty].
    @param focus Focus handle for the box .
    @param focus_attr Style for the border when focused, defaults to [A.fg A.blue].
    @param on_key
      Callback called when a key is pressed while the box is focused. Useful for performing actions when the box is selected . *)
val focusable
  :  ?pad:Nottui_main.gravity
  -> ?pad_w:int
  -> ?pad_h:int
  -> ?label_top:string
  -> ?label_bottom:string
  -> ?border_attr:Notty.attr
  -> ?focus_attr:Notty.attr
  -> ?focus:Nottui_main.Focus.handle
  -> ?on_key:(Nottui_main.Ui.key -> Nottui_main.Ui.may_handle)
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(** Creates a bordered box around the given [input] widget.
    @param scaling
      Controls how the input widget is sized within the border box. Can be:
      - [`Static] - The input widget is not resized.
      - [`Expand sw] - The input widget is allowed to expand to fill the available space, with a stretch width [sw].
      - [`Shrinkable (min_width, sw)] - The input widget is allowed to shrink to a minimum width of [min_width], and expand with a stretch width [sw].
    @param pad The padding around the input widget within the border box.
    @param pad_w The horizontal padding around the input widget.
    @param pad_h The vertical padding around the input widget.
    @param label An optional label to display within the border box.
    @param input The input widget to be bordered.
    @param border_attr Style for the border, defaults to [A.empty]. *)
val box
  :  ?pad:Nottui_main.gravity
  -> ?pad_w:int
  -> ?pad_h:int
  -> ?label_top:string
  -> ?label_bottom:string
  -> ?border_attr:Notty.attr
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(** Creates a bordered box around the given [input]. The input must have a static sive ans this doesn't adjust the s .
    @param scaling
      Controls how the input widget is sized within the border box. Can be:
      - [`Static] - The input widget is not resized.
      - [`Expand sw] - The input widget is allowed to expand to fill the available space, with a stretch width [sw].
      - [`Shrinkable (min_width, sw)] - The input widget is allowed to shrink to a minimum width of [min_width], and expand with a stretch width [sw].
    @param pad The padding around the input widget within the border box.
    @param pad_w The horizontal padding around the input widget.
    @param pad_h The vertical padding around the input widget.
    @param label An optional label to display within the border box.
    @param input The input widget to be bordered.
    @param border_attr Style for the border, defaults to [A.empty]. *)
val static
  :  ?pad:Nottui_main.gravity
  -> ?pad_w:int
  -> ?pad_h:int
  -> ?label_top:Notty.image
  -> ?label_bottom:Notty.image
  -> ?border_attr:Notty.attr
  -> Nottui_main.ui
  -> Nottui_main.ui
