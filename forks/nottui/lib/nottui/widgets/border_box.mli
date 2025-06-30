
(** Creates a bordered box around the given [input] widget.

This box can be focused even if there is no focusable elements inside, it really just wraps the contenst in a [Ui.keyboard_area].

    @param pad_w The horizontal padding around the input widget.
    @param pad_h The vertical padding around the input widget.
    @param label_top An optional label to display at the top of the border box.
    @param label_bottom An optional label to display at the bottom of the border box.
    @param border_attr Style for the border, defaults to [A.empty].
    @param style The style of the border, defaults to [Nottui_main.Ui.Border.unicode].
    @param focus_attr Style for the border when focused, defaults to [A.fg A.blue].
    @param focus_style The style of the border when focused, defaults to [Nottui_main.Ui.Border.unicode_bold].
    @param focus Focus handle for the keyboard focus of the box .
    @param on_key Callback called when a key is pressed while the box is focused. Useful for performing actions when the box is selected. 
    @param input The input widget to be bordered. *)
val focusable
  :  ?pad_w:int
  -> ?pad_h:int
  -> ?label_top:string
  -> ?label_bottom:string
  -> ?border_attr:Notty.attr
  -> ?style:Nottui_main.Ui.Border.style
  -> ?focus_attr:Notty.attr
  -> ?focus_style:Nottui_main.Ui.Border.style
  -> ?focus:Nottui_main.Focus.handle
  -> ?on_key:(Nottui_main.Ui.key -> Nottui_main.Ui.may_handle)
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(** Creates a bordered box around the given [input] widget.
    @param pad_w The horizontal padding around the input widget.
    @param pad_h The vertical padding around the input widget.
    @param label_top An optional label to display at the top of the border box.
    @param label_bottom An optional label to display at the bottom of the border box.
    @param border_attr Style for the border, defaults to [A.empty].
    @param style The style of the border, defaults to [Nottui_main.Ui.Border.unicode].
    @param focus_attr Style for the border when focused, defaults to [A.fg A.blue].
    @param focus_style The style of the border when focused, defaults to [Nottui_main.Ui.Border.unicode_bold].
    @param input The input widget to be bordered. *)
val box
  :  ?pad_w:int
  -> ?pad_h:int
  -> ?label_top:string
  -> ?label_bottom:string
  -> ?border_attr:Notty.attr
  -> ?style:Nottui_main.Ui.Border.style
  -> ?focus_attr:Notty.attr
  -> ?focus_style:Nottui_main.Ui.Border.style
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

