(** Displays a list of ui items.
    If you want the items to be selectable, look at [selection_list] *)
val vlist : ?bullet:string -> Nottui_main.ui Lwd.t list -> Nottui_main.ui Lwd.t

(** Displays a list of something that can be transformed into ui can be filtered.
    Simmilar to selection_list_filterable *)
val vlist_with
  :  ?bullet:string
  -> ?filter:('a -> bool) Lwd.t
  -> ('a -> Nottui_main.ui Lwd.t)
  -> 'a list Lwd.t
  -> Nottui_main.ui Lwd.t
