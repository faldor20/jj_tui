module Exit_status = struct
  [@@@coverage off]

  type t =
    [ `Exited of int
    | `Signaled of int
    ]
  [@@deriving sexp_of]
end

module Lines_or_sexp = struct
  type t =
    | Lines of string list
    | Sexp of Sexp.t

  let sexp_of_t t =
    match t with
    | Lines [] -> [%sexp ""]
    | Lines lines -> [%sexp (lines : string list)]
    | Sexp sexp -> sexp
  ;;

  let create string =
    try Sexp (Parsexp.Conv_single.parse_string_exn string Fn.id) with
    | _ -> Lines (String.split_lines string)
  ;;
end

module Output = struct
  type t =
    { stdout : string
    ; stderr : string
    ; exit_status : Exit_status.t
    }

  let sexp_of_t { stdout; stderr; exit_status } =
    [%sexp
      { stdout = (Lines_or_sexp.create stdout : Lines_or_sexp.t)
      ; stderr = (Lines_or_sexp.create stderr : Lines_or_sexp.t)
      ; exit_status : Exit_status.t
      }]
  ;;

  let exited t ~accept_exit_codes =
    match
      match t.exit_status with
      | `Exited code -> List.Assoc.find accept_exit_codes ~equal:Int.equal code
      | `Signaled _ -> None
    with
    | Some a -> Ok a
    | None ->
      Or_error.error_s
        [%sexp
          "unexpected exit status"
          , { accept_exit_codes = (List.map accept_exit_codes ~f:fst : int list) }]
  ;;

  let exit ?(accept_nonzero_exit = []) t =
    exited
      t
      ~accept_exit_codes:
        ((0, ()) :: List.map accept_nonzero_exit ~f:(fun code -> code, ()))
  ;;

  let exit_and_stdout ?accept_nonzero_exit t =
    let%map () = exit t ?accept_nonzero_exit in
    t.stdout
  ;;

  let expect_no_output ?(accept_nonzero_exit = []) t =
    let%bind stdout = exit_and_stdout t ~accept_nonzero_exit in
    if String.is_empty stdout then Ok () else Or_error.error_string "expected no output"
  ;;
end

exception User_error of Error.t

let run ~process_mgr ~cwd ?stdin ?env ~prog ~args () ~f =
  Eio.Switch.run
  @@ fun sw ->
  let r, w = Eio.Process.pipe process_mgr ~sw in
  let re, we = Eio.Process.pipe process_mgr ~sw in
  let exit_status_r : [ Exit_status.t | `Unknown ] ref = ref `Unknown in
  let stdout_r = ref "" in
  let stderr_r = ref "" in
  try
    let child =
      Eio.Process.spawn
        ~sw
        process_mgr
        ~cwd
        ?stdin
        ~stdout:w
        ~stderr:we
        ?env
        ?executable:None
        (prog :: args)
    in
    Eio.Flow.close w;
    Eio.Flow.close we;
    let stdout = Eio.Buf_read.parse_exn Eio.Buf_read.take_all r ~max_size:Int.max_value in
    stdout_r := stdout;
    let stderr =
      Eio.Buf_read.parse_exn Eio.Buf_read.take_all re ~max_size:Int.max_value
    in
    stderr_r := stderr;
    Eio.Flow.close r;
    let exit_status = Eio.Process.await child in
    exit_status_r := (exit_status :> [ Exit_status.t | `Unknown ]);
    match f { Output.stdout; stderr; exit_status } with
    | Ok _ as ok -> ok
    | Error err -> raise (User_error err)
  with
  | (Eio.Exn.Io _ | User_error _) as exn ->
    let error =
      match exn with
      | Eio.Exn.Io _ -> Error.of_exn exn
      | User_error error -> error
      | _ -> assert false
    in
    Or_error.error_s
      [%sexp
        { prog : string
        ; args : string list
        ; exit_status = (!exit_status_r : [ Exit_status.t | `Unknown ])
        ; cwd = (snd cwd : string)
        ; stdout = (Lines_or_sexp.create !stdout_r : Lines_or_sexp.t)
        ; stderr = (Lines_or_sexp.create !stderr_r : Lines_or_sexp.t)
        ; error : Error.t
        }]
;;

let run_stdout ~process_mgr ~cwd ?stdin ?accept_nonzero_exit ?env ~prog ~args () =
  run ~process_mgr ~cwd ?stdin ?env ~prog ~args () ~f:(fun output ->
    Output.exit_and_stdout output ?accept_nonzero_exit)
;;

let run_lines ~process_mgr ~cwd ?stdin ?accept_nonzero_exit ?env ~prog ~args () =
  run ~process_mgr ~cwd ?stdin ?env ~prog ~args () ~f:(fun output ->
    Output.exit_and_stdout output ?accept_nonzero_exit >>| String.split_lines)
;;

let run_expect_no_output ~process_mgr ~cwd ?stdin ?accept_nonzero_exit ?env ~prog ~args ()
  =
  run ~process_mgr ~cwd ?stdin ?env ~prog ~args () ~f:(fun output ->
    Output.expect_no_output output ?accept_nonzero_exit)
;;
