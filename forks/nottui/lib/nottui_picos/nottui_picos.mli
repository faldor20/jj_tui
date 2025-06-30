open Notty
open Nottui
open Picos

module Ui_loop : sig
  open Notty_unix

  (** Run one step of the main loop.

      Update output image describe by the provided [root].
      If [process_event], wait up to [timeout] seconds for an input event, then
      consume and dispatch it.

       *)
  (*
     val step:
    Picos_io_fd.t
    -> ?process_event:bool
    -> ?timeout:float
    -> renderer:Renderer.t
    -> Term.t
    -> ui Lwd.root
    -> unit
  *)

  (*
     NOTE: Currently we use a tick and a timeout, this is essentially how long we will wait to respond to events that happened
  *)
  (** Repeatedly run steps of the main loop, until either:
      - [quit] becomes true,
      - the ui computation raises an exception,
      - if [quit_on_ctrl_q] was true or not provided, wait for Ctrl-Q event
      - if [quit_on_escape] was true or not provided, wait for Escape event

      Specific [term] or [renderer] instances can be provided, otherwise new
      ones will be allocated and released.

     Uses Picos for concurrency. 
     The only change vs the normal version is this yields whenever waiting for input 
       *)

  val run
    :  ?on_invalidate:(ui -> unit)
    -> ?tick:(unit -> unit)
    -> ?term:Term.t
    -> ?renderer:Renderer.t
    -> ?quit:bool Lwd.var
    -> ?quit_on_escape:bool
    -> ?quit_on_ctrl_q:bool
    -> ui Lwd.t
    -> unit
end

module Widgets : sig
  (**
    Async widgets are widgets that use some kind of async mechanism to update themselves.
    They are only available when using picos.
*)

  (** A throbber that spins forever. 

Each render it forks a thread to update the throbber to the next frame in 100ms*)
  val throbber : Nottui.ui Lwd.t
end
