let%expect_test "run" =
  (* Returning the [Output.t]. *)
  Eio_main.run
  @@ fun env ->
  let result =
    Eio_process.run
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~stdin:(Eio.Flow.string_source "Hello World!")
      ~prog:"cat"
      ~args:[]
      ()
      ~f:Or_error.return
  in
  print_s [%sexp (result : Eio_process.Output.t Or_error.t)];
  [%expect {| (Ok ((stdout ("Hello World!")) (stderr "") (exit_status (Exited 0)))) |}];
  (* Executable not found. *)
  let result =
    Eio_process.run
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~stdin:(Eio.Flow.string_source "Hello World!")
      ~prog:"invalid-program-not-found-314"
      ~args:[ "foo"; "bar" ]
      ()
      ~f:Or_error.return
  in
  print_s [%sexp (result : Eio_process.Output.t Or_error.t)];
  [%expect
    {|
    (Error (
      (prog invalid-program-not-found-314)
      (args (foo bar))
      (exit_status Unknown)
      (cwd         "")
      (stdout      "")
      (stderr      "")
      (error (
        "Eio.Io Process Executable \"invalid-program-not-found-314\" not found")))) |}];
  (* Accept nonzero exit code. *)
  let result =
    Eio_process.run_stdout
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[]
      ()
  in
  print_s [%sexp (result : string Or_error.t)];
  [%expect {| (Ok "") |}];
  let result =
    Eio_process.run_stdout
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[ "--exit-code"; "128" ]
      ()
  in
  print_s [%sexp (result : string Or_error.t)];
  [%expect
    {|
    (Error (
      (prog ./bin/main.exe)
      (args        (--exit-code 128))
      (exit_status (Exited      128))
      (cwd    "")
      (stdout "")
      (stderr "")
      (error ("unexpected exit status" ((accept_exit_codes (0))))))) |}];
  let result =
    Eio_process.run_stdout
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~accept_nonzero_exit:[ 128 ]
      ~prog:"./bin/main.exe"
      ~args:[ "--exit-code"; "128" ]
      ()
  in
  print_s [%sexp (result : string Or_error.t)];
  [%expect {| (Ok "") |}];
  (* Signal. *)
  let result =
    Eio_process.run_stdout
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[ "--signal" ]
      ()
  in
  print_s [%sexp (result : string Or_error.t)];
  [%expect
    {|
    (Error (
      (prog ./bin/main.exe)
      (args (--signal))
      (exit_status (Signaled -7))
      (cwd    "")
      (stdout "")
      (stderr "")
      (error ("unexpected exit status" ((accept_exit_codes (0))))))) |}];
  (* Run lines. *)
  let result =
    Eio_process.run_lines
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[]
      ()
  in
  print_s [%sexp (result : string list Or_error.t)];
  [%expect {| (Ok ()) |}];
  let result =
    Eio_process.run_lines
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[ "--stdout" ]
      ()
  in
  print_s [%sexp (result : string list Or_error.t)];
  [%expect
    {|
    (Ok (
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas quis nisi id"
      "lorem scelerisque bibendum eget id felis. Pellentesque consectetur tincidunt"
      ornare.)) |}];
  (* Run expect no output. *)
  let result =
    Eio_process.run_expect_no_output
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[ "--stderr" ]
      ()
  in
  print_s [%sexp (result : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  let result =
    Eio_process.run_expect_no_output
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[ "--stdout" ]
      ()
  in
  print_s [%sexp (result : unit Or_error.t)];
  [%expect
    {|
    (Error (
      (prog ./bin/main.exe)
      (args (--stdout))
      (exit_status (Exited 0))
      (cwd "")
      (stdout (
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas quis nisi id"
        "lorem scelerisque bibendum eget id felis. Pellentesque consectetur tincidunt"
        ornare.))
      (stderr "")
      (error  "expected no output"))) |}];
  (* User further processing the output. *)
  let result =
    Eio_process.run
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[ "--stdout"; "--stderr"; "--output-sexp"; "--exit-code"; "128" ]
      ()
      ~f:(fun output ->
        match%map
          Eio_process.Output.exited output ~accept_exit_codes:[ 0, `Zero; 1, `One ]
        with
        | `Zero | `One -> assert false)
  in
  (* When the user function [f] returned an Error, we include all the info in
     the error message. *)
  print_s [%sexp (result : int Or_error.t)];
  [%expect
    {|
    (Error (
      (prog ./bin/main.exe)
      (args (--stdout --stderr --output-sexp --exit-code 128))
      (exit_status (Exited 128))
      (cwd "")
      (stdout ((words (Lorem ipsum dolor sit amet))))
      (stderr ())
      (error ("unexpected exit status" ((accept_exit_codes (0 1))))))) |}];
  let result =
    Eio_process.run
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Stdenv.cwd env)
      ~prog:"./bin/main.exe"
      ~args:[ "--stdout"; "--output-sexp"; "--exit-code"; "1" ]
      ()
      ~f:(fun output ->
        match%map
          Eio_process.Output.exited output ~accept_exit_codes:[ 0, `Zero; 1, `One ]
        with
        | `One -> 1
        | `Zero -> assert false)
  in
  print_s [%sexp (result : int Or_error.t)];
  [%expect {| (Ok 1) |}];
  ()
;;

let%expect_test "lines" =
  (* We monitor this as these results influence the implementation [Lines_or_sexp]. *)
  print_s [%sexp (String.split ~on:'\n' "" : string list)];
  [%expect {| ("") |}];
  print_s [%sexp (String.split_lines "" : string list)];
  [%expect {| () |}];
  ()
;;
