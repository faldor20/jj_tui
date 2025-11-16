(*******
parser tests
*******)
open Notty
open AnsiReverse

module ParserTests = struct
  open AnsiReverse.Parser
  open AnsiReverse.Internal

  let%expect_test "escape_parser" =
    let test_str = "\027[32m" in
    let res =
      Angstrom.parse_string ~consume:All parse_escape_seq test_str |> Result.get_ok
    in
    print_escape_seq res;
    [%expect
      {|
  escape sequence:
  attr:
  \e[0m<\e[0;32mATTR\e[0m\e[K\e[0m>\e[0m
  |}]
  ;;

  let%expect_test "parse_ansi_escape_codes_test" =
    let test_str = "\027[4m\027[38;5;1m\"success\"\027[38;5;2mNone\027[24m\027[39mrest" in
    (match parse_ansi_escape_codes test_str with
     | Error err ->
       Printf.printf "Error: %s\n" err
     | Ok result ->
       Printf.printf "Parsed %d segments:\n" (List.length result);
       List.iter
         (fun (attr, text) ->
            print_attr attr;
            Printf.printf "Text: \"%s\"\n" (String.escaped text))
         result);
    [%expect
      {|
    Parsed 4 segments:
    attr:
    \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
    Text: ""
    attr:
    \e[0m<\e[0;31;4mATTR\e[0m\e[K\e[0m>\e[0m
    Text: "\"success\""
    attr:
    \e[0m<\e[0;32;4mATTR\e[0m\e[K\e[0m>\e[0m
    Text: "None"
    attr:
    \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
    Text: "rest"
    |}]
  ;;

  let%expect_test "parse_ansi_strikethrough_test" =
    let open A in
    print_attr (A.st A.strike);
    print_attr (A.st A.strike ++ A.fg A.red);
    print_attr (A.st A.blink);
    print_attr (A.st A.dim);
    print_attr (A.st A.italic);
    print_attr (A.st A.underline);
    print_attr (A.st A.bold);
    print_attr (A.st A.reverse ++ A.fg A.red);
    print_attr (A.st A.hidden);
    [%expect
      {|
    attr:
    \e[0m<\e[0;9mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;31;9mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;5mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;2mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;3mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;4mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;1mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;31;7mATTR\e[0m\e[K\e[0m>\e[0m
    attr:
    \e[0m<\e[0;8mATTR\e[0m\e[K\e[0m>\e[0m
    |}]
  ;;

  let%expect_test "attribute_removal_test" =
    let open A in
    (* Test removing styles *)
    let base_attr = st bold ++ st underline ++ st italic in
    let result = base_attr -- st italic in
    Internal.print_attr result;
    [%expect
      {|
    attr:
    \e[0m<\e[0;1;4mATTR\e[0m\e[K\e[0m>\e[0m
    |}];
    (* Test removing multiple styles at once *)
    let result2 = base_attr -- (st underline ++ st italic) in
    Internal.print_attr result2;
    [%expect
      {|
    attr:
    \e[0m<\e[0;1mATTR\e[0m\e[K\e[0m>\e[0m
    |}];
    (* Test removing foreground color *)
    let colored = base_attr ++ fg red in
    let color_reset = colored -- fg color_reset in
    Internal.print_attr color_reset;
    [%expect
      {|
    attr:
    \e[0m<\e[0;1;3;4mATTR\e[0m\e[K\e[0m>\e[0m
    |}];
    (* Test removing a style from colored *)
    let no_underline = colored -- st underline in
    Internal.print_attr no_underline;
    [%expect
      {|
    attr:
    \e[0m<\e[0;31;1;3mATTR\e[0m\e[K\e[0m>\e[0m
    |}];
    (* Test removing background color *)
    let bg_colored = base_attr ++ bg blue in
    let no_bg = bg_colored -- bg blue in
    Internal.print_attr no_bg;
    [%expect
      {|
    attr:
    \e[0m<\e[0;1;3;4mATTR\e[0m\e[K\e[0m>\e[0m
    |}];
    (* Test resetting to empty *)
    let full_attr = st bold ++ fg red ++ bg green in
    let empty_result = full_attr -- full_attr in
    Internal.print_attr empty_result;
    [%expect
      {|
    attr:
    \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
    |}]
  ;;

  let%expect_test "edge_case_1" =
    let test_str = {|[38;5;2m 145[39m: [4m[38;5;2mone[24mscript[4m10|} in
    (match parse_ansi_escape_codes test_str with
     | Error err ->
       Printf.printf "Error: %s\n" err
     | Ok result ->
       Printf.printf "Parsed %d segments:\n" (List.length result);
       List.iter
         (fun (attr, text) ->
            print_attr attr;
            Printf.printf "Text: %s" (String.escaped text))
         result);
    [%expect
      {|
    Parsed 6 segments:
    attr:
    \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
    Text: attr:
    \e[0m<\e[0;32mATTR\e[0m\e[K\e[0m>\e[0m
    Text:  145attr:
    \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
    Text: : attr:
    \e[0m<\e[0;32;4mATTR\e[0m\e[K\e[0m>\e[0m
    Text: oneattr:
    \e[0m<\e[0;32mATTR\e[0m\e[K\e[0m>\e[0m
    Text: scriptattr:
    \e[0m<\e[0;32;4mATTR\e[0m\e[K\e[0m>\e[0m
    Text: 10
    |}]
  ;;

  let%expect_test "parse_delta_test" =
    let test_str =
      "\027[48;2;0;40;0;38;2;248;248;242mhi-there[0m[48;2;0;40;0m[0K[0mhi"
    in
    (match parse_ansi_escape_codes test_str with
     | Error err ->
       Printf.printf "Error: %s\n" err
     | Ok result ->
       Printf.printf "Parsed %d segments:\n" (List.length result);
       List.iter
         (fun (attr, text) ->
            print_attr attr;
            Printf.printf "Text: '%s'\n" (String.escaped text))
         result);
    [%expect
      {|
  Parsed 3 segments:
  attr:
  \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
  Text: ''
  attr:
  \e[0m<\e[0;38;2;248;248;242;48;2;0;40;0mATTR\e[0m\e[K\e[0m>\e[0m
  Text: 'hi-there'
  attr:
  \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
  Text: 'hi'
  |}]
  ;;

  let%expect_test "parse_delta_test 2" =
    (*turns out this was really testing the \e[0k sequence, which would break anything after it*)
    let test_str =
      {|[0K[0m
[48;2;0;250;250;38;2;248;248;242m                (x[0m[7;35m [0m[48;2;0;40;0m[0K[0m|}
    in
    (match parse_ansi_escape_codes test_str with
     | Error err ->
       Printf.printf "Error: %s\n" err
     | Ok result ->
       Printf.printf "Parsed %d segments:\n" (List.length result);
       List.iter
         (fun (attr, text) ->
            print_attr attr;
            Printf.printf "Text: '%s'\n" (String.escaped text))
         result);
    [%expect
      {|
  Parsed 5 segments:
  attr:
  \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
  Text: ''
  attr:
  \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
  Text: '\n'
  attr:
  \e[0m<\e[0;38;2;248;248;242;48;2;0;250;250mATTR\e[0m\e[K\e[0m>\e[0m
  Text: '                (x'
  attr:
  \e[0m<\e[0;35;7mATTR\e[0m\e[K\e[0m>\e[0m
  Text: ' '
  attr:
  \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
  Text: ''
  |}]
  ;;
end

module ImageTests = struct
  (*============
     Tests
=============*)

  (* Test delta output*)
  let%expect_test "multiple escape sequences test" =
    let test_str = "\027[48;2;0;40;0;38;2;248;248;242m" in
    let res =
      Angstrom.parse_string ~consume:All Parser.parse_escape_seq test_str |> Result.get_ok
    in
    Parser.print_escape_seq res;
    (* print_endline test_str; *)
    [%expect
      {|
  escape sequence:
  attr:
  \e[0m<\e[0;48;2;0;40;0mATTR\e[0m\e[K\e[0m>\e[0m
  attr:
  \e[0m<\e[0;38;2;248;248;242mATTR\e[0m\e[K\e[0m>\e[0m
  |}]
  ;;

  let%expect_test "delta test output" =
    let test_str =
      "\027[48;2;0;40;0;38;2;248;248;242mhi-there[0m[48;2;0;40;0m[0K[0mhi"
    in
    let img = colored_string test_str in
    Internal.print_image_escaped img;
    (* print_endline test_str; *)
    [%expect
      {|
  image:
  \027[0;38;2;248;248;242;48;2;0;40;0mhi-there\027[0m\027[K\027[0mhi\027[0m
  |}]
  ;;

  let%expect_test "delta test output 2" =
    let test_str =
      {|
[48;2;63;0;1m                (x [48;2;144;16;17m^ "\n"[0m[48;2;63;0;1m[0K[0m
[48;2;0;250;250;38;2;248;248;242m                (x[0m[7;35m [0m[48;2;0;40;0m[0K[0m
|}
    in
    let img = colored_string test_str in
    Internal.print_image img;
    [%expect
      {|
  image:
  
                  (x ^ "\n"
                  (x 
                           
  |}]
  ;;
end
