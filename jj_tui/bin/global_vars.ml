open Notty
open Nottui
open Eio.Std

type cmd_args = string list

type ui_state_t = {
  view :
    [ `Main
    | `Cmd_I of cmd_args
    | `RunCmd of
      cmd_args
      (* | `Prompt of string * [ `Cmd of cmd_args | `Cmd_I of cmd_args ] *)
    ]
      Lwd.var;
  input : [ `Normal | `Mode of char -> Ui.may_handle ] Lwd.var;
  show_popup : (ui Lwd.t * string) option Lwd.var;
  show_prompt :
    (string * string * ([ `Finished of string | `Closed ] -> unit)) option Lwd.var;
  command_log : string list Lwd.var;
  jj_tree : I.t Lwd.var;
  jj_show : I.t Lwd.var;
  jj_branches : I.t Lwd.var;
  jj_change_files : (string * string) list Lwd.var;
}

(** Global variables for the ui*)
module type Vars = sig
  type eio_vars = {
    env : Eio_unix.Stdenv.base;
    mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
    cwd : Eio.Fs.dir_ty Eio.Path.t;
  }

  val quit : bool Lwd.var
  val term : Notty_unix.Term.t option ref
  val term_width_height : (int * int) Lwd.var
  val get_eio_env : unit -> Eio_unix.Stdenv.base
  val set_eio_env :Eio_unix.Stdenv.base->unit
  val get_eio_vars : unit -> eio_vars
  val get_term : unit -> Notty_unix.Term.t
  val ui_state : ui_state_t
end

module Vars : Vars = struct
  type eio_vars = {
    env : Eio_unix.Stdenv.base;
    mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
    cwd : Eio.Fs.dir_ty Eio.Path.t;
  }

  let quit = Lwd.var false
  let eio = ref None

  let ui_state =
    {
      view = Lwd.var `Main;
      jj_tree = Lwd.var I.empty;
      jj_show = Lwd.var I.empty;
      jj_branches = Lwd.var I.empty;
      jj_change_files = Lwd.var [];
      input = Lwd.var `Normal;
      show_popup = Lwd.var None;
      show_prompt = Lwd.var None;
      command_log = Lwd.var [];
    }
  ;;

  let set_eio_env env =
    eio := Some { env; mgr = Eio.Stdenv.process_mgr env; cwd = Eio.Stdenv.cwd env }
  ;;

  let term = ref None
  let term_width_height : (int * int) Lwd.var = Lwd.var (0, 0)
  let get_eio_env () = (Option.get !eio).env
  let get_eio_vars () = Option.get !eio
  let get_term () = Option.get !term

end
