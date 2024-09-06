open Notty
open Nottui
open Eio.Std
open Picos_std_structured
open Lwd_infix
open Jj_tui.Process

type cmd_args = string list

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
  ; show_prompt : W.Overlay.text_prompt_data option Lwd.var
      (* ; show_graph_selection_prompt : *)
      (* rev_id maybe_unique W.Overlay.filterable_selection_list_prompt_data option Lwd.var *)
  ; show_string_selection_prompt :
      string W.Overlay.filterable_selection_list_prompt_data option Lwd.var
  ; graph_revs : rev_id maybe_unique W.Lists.selectable_item array Lwd.var
  ; command_log : string list Lwd.var
  ; jj_tree : I.t Lwd.var
  ; jj_show : I.t Lwd.var
  ; jj_show_promise : (unit Promise.t) ref
  ; jj_branches : I.t Lwd.var
  ; jj_change_files : (string * string) list Lwd.var
  ; selected_revision : rev_id maybe_unique Lwd.var
  ; revset : string option Lwd.var
  ; trigger_update : unit Lwd.var
}

let get_unique_id maybe_unique_rev =
  match maybe_unique_rev with
  | Unique { change_id; _ } ->
    change_id
  | Duplicate { commit_id; _ } ->
    commit_id
;;

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
    ; jj_show_promise = ref @@ Promise.of_value ()
    ; jj_branches = Lwd.var I.empty
    ; jj_change_files = Lwd.var []
    ; selected_revision = Lwd.var (Unique { change_id = "@"; commit_id = "@" })
    ; revset = Lwd.var None
    ; graph_revs = Lwd.var [||]
    ; input = Lwd.var `Normal
    ; show_popup = Lwd.var None
    ; show_prompt = Lwd.var None
    ; show_string_selection_prompt = Lwd.var None
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
  let get_selected_rev () = Lwd.peek ui_state.selected_revision |> get_unique_id

  (**see [get_selected_rev]*)
  let get_selected_rev_lwd () =
    let$ a = Lwd.get ui_state.selected_revision in
    a |> get_unique_id
  ;;
end
