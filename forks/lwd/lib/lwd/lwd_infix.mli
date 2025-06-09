(*BEGIN LETOP*)
val (let$) : 'a Lwd.t -> ('a -> 'b) -> 'b Lwd.t
(** Alias to {!Lwd.map'} suitable for let-op bindings *)

val (let$*) : 'a Lwd.t -> ('a -> 'b Lwd.t) -> 'b Lwd.t
(** Alias to {!Lwd.bind} suitable for let-op bindings *)

val (and$) : 'a Lwd.t -> 'b Lwd.t -> ('a * 'b) Lwd.t
(** Alias to {!Lwd.pair} suitable for let-op bindings *)
(*END*)

val ($=) : 'a Lwd.var -> 'a -> unit
(** Infix alias to {!Lwd.set} *)

val ($<-) : 'a Lwd_table.row -> 'a -> unit
(** Infix alias to {!Lwd_table.set} *)

val ( |>$ ):'a Lwd.t -> ('a -> 'b) -> 'b Lwd.t 
(** Used to pipe with Lwd.map *)

val ( >> ):('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Function composition operator *)

