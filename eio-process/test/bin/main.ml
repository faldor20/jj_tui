let exit_code = ref 0
let stdout = ref false
let stderr = ref false
let output_sexp = ref false
let signal = ref false

let spec_list =
  [ "--exit-code", Stdlib.Arg.Set_int exit_code, " Exit with given code"
  ; "--stdout", Stdlib.Arg.Set stdout, " Write to stdout"
  ; "--stderr", Stdlib.Arg.Set stderr, " Write to stderr"
  ; "--output-sexp", Stdlib.Arg.Set output_sexp, " Make output format a s-expression"
  ; "--signal", Stdlib.Arg.Set signal, " Send a kill signal to itself"
  ]
;;

let lorem_ipsum =
  lazy
    ({|
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas quis nisi id
lorem scelerisque bibendum eget id felis. Pellentesque consectetur tincidunt
ornare.
|}
     |> String.strip)
;;

let sexp = lazy [%sexp { words = [ "Lorem"; "ipsum"; "dolor"; "sit"; "amet" ] }]

let () =
  Stdlib.Arg.parse spec_list ignore "test";
  if !stdout
  then
    if !output_sexp
    then Stdlib.print_endline (force sexp |> Sexp.to_string_hum)
    else Stdlib.print_endline (force lorem_ipsum);
  if !stderr
  then
    if !output_sexp
    then Stdlib.prerr_endline "()"
    else Stdlib.prerr_endline (force lorem_ipsum);
  if !signal then Unix.kill (Unix.getpid ()) Stdlib.Sys.sigkill [@coverage off];
  Stdlib.exit !exit_code [@coverage off]
;;
