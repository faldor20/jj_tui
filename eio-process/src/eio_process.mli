(** Spawn external processes in [Eio] with convenient wrappers inspired by
    [Async_unix.Process]. *)

module Exit_status : sig
  type t =
    [ `Exited of int
    | `Signaled of int
    ]
  [@@deriving sexp_of]
end

module Output : sig
  type t =
    { stdout : string
    ; stderr : string
    ; exit_status : Exit_status.t
    }
  [@@deriving sexp_of]

  val exit : ?accept_nonzero_exit:int list -> t -> unit Or_error.t
  val exit_and_stdout : ?accept_nonzero_exit:int list -> t -> string Or_error.t
  val exited : t -> accept_exit_codes:(int * 'a) list -> 'a Or_error.t
  val expect_no_output : ?accept_nonzero_exit:int list -> t -> unit Or_error.t
end

val run
  :  process_mgr:_ Eio.Process.mgr
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> ?stdin:_ Eio.Flow.source
  -> ?env:string array
  -> prog:string
  -> args:string list
  -> unit
  -> f:(Output.t -> 'a Or_error.t)
  -> 'a Or_error.t

val run_stdout
  :  process_mgr:_ Eio.Process.mgr
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> ?stdin:_ Eio.Flow.source
  -> ?accept_nonzero_exit:int list
  -> ?env:string array
  -> prog:string
  -> args:string list
  -> unit
  -> string Or_error.t

val run_lines
  :  process_mgr:_ Eio.Process.mgr
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> ?stdin:_ Eio.Flow.source
  -> ?accept_nonzero_exit:int list
  -> ?env:string array
  -> prog:string
  -> args:string list
  -> unit
  -> string list Or_error.t

val run_expect_no_output
  :  process_mgr:_ Eio.Process.mgr
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> ?stdin:_ Eio.Flow.source
  -> ?accept_nonzero_exit:int list
  -> ?env:string array
  -> prog:string
  -> args:string list
  -> unit
  -> unit Or_error.t
