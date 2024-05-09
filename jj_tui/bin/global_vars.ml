open Notty
open Nottui

type ui_state_t = {
  view :
    [ `Main
    | `Cmd of string list
    | `RunCmd of string list
    | `Prompt of string * string list
    ]
      Lwd.var;
  input : [ `Normal | `Mode of char -> Ui.may_handle ] Lwd.var;
  command_log: string list Lwd.var;
  jj_tree : I.t Lwd.var;
  jj_show : I.t Lwd.var;
}

(** Global variables for the ui*)
module type Vars = sig
  val quit : bool Lwd.var
  val eio_env : Eio_unix.Stdenv.base option ref
  val term : Notty_unix.Term.t option ref
  val term_width_height : (int * int) Lwd.var
  val get_eio_env : unit -> Eio_unix.Stdenv.base
  val get_term : unit -> Notty_unix.Term.t
  val ui_state : ui_state_t
end

module Vars : Vars = struct
  let quit = Lwd.var false
  let eio_env = ref None

  let ui_state =
    {
      view = Lwd.var `Main;
      jj_tree = Lwd.var I.empty;
      jj_show = Lwd.var I.empty;
      input = Lwd.var `Normal;
      command_log= Lwd.var []
    }
  ;;

  let term = ref None
  let term_width_height : (int * int) Lwd.var = Lwd.var (0, 0)
  let get_eio_env () = Option.get !eio_env
  let get_term () = Option.get !term
end
