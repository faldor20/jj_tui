(** Shows the size of the ui provided. Useful for debugging*)
val size_logger : Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t

(**Sets an attr for anything behind the given area*)
val set_bg : attr:Notty.attr -> Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t

(**Clears anything behind the given area using the width. If you have a dynamic sized element use [set_bg]*)
val set_bg_static : attr:Notty.attr -> Nottui_main.ui -> Nottui_main.ui

(**Clears anything behind the given area*)
val clear_bg : Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t

(** Config for a text_prompt*)
type text_prompt_data =
  { label : string
  ; pre_fill : string
  ; on_exit : [ `Closed | `Finished of string ] -> unit
  }

(** Text box prompt that takes user input then calls [on_exit] with the result.

This will display ontop of any ui it is passed when show_prompt_var is [Some].*)

val text_prompt
  :  ?pad_h:int
  -> ?pad_w:int
  -> ?modify_body:(Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t)
  -> ?focus:Nottui_main.Focus.handle
  -> ?char_count:bool
  -> show_prompt_var:text_prompt_data option Lwd.var
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(** Config for a selection_list_prompt*)
type 'a selection_list_prompt_data =
  { label : string
  ; items : 'a Selection_list.multi_selectable_item list Lwd.t
  ; on_exit : [ `Closed | `Finished of 'a ] -> unit
  }

(** Selection_list prompt.

This will display ontop of any ui it is passed when show_prompt_var is [Some].
@param modify_body Function that takes the completed body of the prompt, incase you want to resize it or otherwise change it
*)
val selection_list_prompt
  :  ?pad_w:int
  -> ?pad_h:int
  -> ?modify_body:(Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t)
  -> ?focus:Nottui_main.Focus.handle
  -> show_prompt_var:'a selection_list_prompt_data option Lwd.var
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

type 'a filterable_selection_list_prompt_data =
  { label : string
  ; items : 'a Selection_list.multi_selectable_item list Lwd.t
  ; filter_predicate : string -> 'a -> bool
  ; on_exit : [ `Closed | `Finished of 'a ] -> unit
  }

(** Selection_list prompt that is filterable.

This will display ontop of any ui it is passed when show_prompt_var is [Some].
@param modify_body Function that takes the completed body of the prompt, incase you want to resize it or otherwise change it
*)
val selection_list_prompt_filterable
  :  ?pad_w:int
  -> ?pad_h:int
  -> ?list_outline_focus_attr:Notty.A.t
  -> ?modify_body:(Nottui_main.ui Lwd.t -> Nottui_main.ui Lwd.t)
  -> ?focus:Nottui_main.Focus.handle
  -> show_prompt_var:'a filterable_selection_list_prompt_data option Lwd.var
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t

(**This is a simple popup that can show ontop of other ui elements *)
val popup
  :  ?focus:Nottui_main.Focus.handle
  -> ?on_key:(Nottui_main.Ui.key -> Nottui_main.Ui.may_handle)
  -> show_popup_var:(Nottui_main.ui Lwd.t  * string) option Lwd.var
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t
(**This is a popup that can show ontop of other ui elements
It passes the focus handler into the contained ui element. 
useful for making a popup that contains an element that accepts keyboard input *)
val popup_focusable
  :  ?focus:Nottui_main.Focus.handle
  -> ?on_key:(Nottui_main.Ui.key -> Nottui_main.Ui.may_handle)
  -> show_popup_var:((focus:Nottui_main.Focus.handle -> Nottui_main.ui Lwd.t)  * string) option Lwd.var
  -> Nottui_main.ui Lwd.t
  -> Nottui_main.ui Lwd.t
