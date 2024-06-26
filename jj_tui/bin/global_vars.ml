open Notty
open Nottui
open Eio.Std
open Lwd_infix

type cmd_args = string list

type rev_id = {
    change_id : string
  ; commit_id : string
}

type 'a maybe_unique =
  | Unique of 'a
  | Duplicate of 'a

type ui_state_t = {
    view :
      [ `Main (**Normal Mode*)
      | `Cmd_I of cmd_args (**Indicates we are running a JJ command that is interactive*)
      | `RunCmd of
        cmd_args
        (* | `Prompt of string * [ `Cmd of cmd_args | `Cmd_I of cmd_args ] *)
      ]
        Lwd.var
  ; input : [ `Normal | `Mode of char -> Ui.may_handle ] Lwd.var
  ; show_popup : (ui Lwd.t * string) option Lwd.var
  ; show_prompt :
      (string * string * ([ `Finished of string | `Closed ] -> unit)) option Lwd.var
  ; command_log : string list Lwd.var
  ; jj_tree : I.t Lwd.var
  ; jj_show : I.t Lwd.var
  ; jj_branches : I.t Lwd.var
  ; jj_change_files : (string * string) list Lwd.var
  ; selected_revision : rev_id maybe_unique Lwd.var
  ; trigger_update : unit Lwd.var
}

(** Global variables for the ui. Here we keep anything that's just a pain to pipe around*)
module type Vars = sig
  type eio_vars = {
      env : Eio_unix.Stdenv.base
    ; mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
    ; cwd : Eio.Fs.dir_ty Eio.Path.t
  }

  val quit : bool Lwd.var
  val term : Notty_unix.Term.t option ref
  val term_width_height : (int * int) Lwd.var
  val get_eio_env : unit -> Eio_unix.Stdenv.base
  val set_eio_env : Eio_unix.Stdenv.base -> unit
  val get_eio_vars : unit -> eio_vars
  val get_term : unit -> Notty_unix.Term.t
  val ui_state : ui_state_t
  val update_ui_state : (ui_state_t -> unit) -> unit
  val render_mutex : Eio.Mutex.t

  (**returns either a change_id or if their are change_id conflicts, a commit_id *)
  val get_selected_rev : unit -> string

  (**returns either a change_id or if their are change_id conflicts, a commit_id *)
  val get_selected_rev_lwd : unit -> string Lwd.t
end

module Vars : Vars = struct
  type eio_vars = {
      env : Eio_unix.Stdenv.base
    ; mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
    ; cwd : Eio.Fs.dir_ty Eio.Path.t
  }

  let quit = Lwd.var false
  let eio = ref None

  (** DONT DIRECTLY SET FROM EIO FIBERS!! When setting variables within a fiber use update_ui_state*)
  let ui_state =
    {
      view = Lwd.var `Main
    ; jj_tree = Lwd.var I.empty
    ; jj_show = Lwd.var I.empty
    ; jj_branches = Lwd.var I.empty
    ; jj_change_files = Lwd.var []
    ; selected_revision = Lwd.var (Unique { change_id = "@"; commit_id = "@" })
    ; input = Lwd.var `Normal
    ; show_popup = Lwd.var None
    ; show_prompt = Lwd.var None
    ; command_log = Lwd.var []
    ; trigger_update = Lwd.var ()
    }
  ;;

  (** allows other fibers to set lwd vars only when not during rendering*)
  let render_mutex = Eio.Mutex.create ()

  (** Safely ensures your update doesn't occur during a render*)
  let update_ui_state f =
    Eio.Mutex.lock render_mutex;
    f ui_state;
    Eio.Mutex.unlock render_mutex
  ;;

  let set_eio_env env =
    eio := Some { env; mgr = Eio.Stdenv.process_mgr env; cwd = Eio.Stdenv.cwd env }
  ;;

  let term = ref None
  let term_width_height : (int * int) Lwd.var = Lwd.var (0, 0)
  let get_eio_env () = (Option.get !eio).env
  let get_eio_vars () = Option.get !eio
  let get_term () = Option.get !term

  (**Gets an id for the selected revision. If the change_id is unique we use that, if it's not we return a commit_id instead*)
  let get_selected_rev () =
    match Lwd.peek ui_state.selected_revision with
    | Unique { change_id; _ } ->
      change_id
    | Duplicate { commit_id; _ } ->
      commit_id
  ;;

  (**see [get_selected_rev]*)
  let get_selected_rev_lwd () =
    let$ a = Lwd.get ui_state.selected_revision in
    match a with
    | Unique { change_id; _ } ->
      change_id
    | Duplicate { commit_id; _ } ->
      commit_id
  ;;
end
