open Notty

module Internal = struct
  type op = Buffer.t -> unit

  let invalid_arg fmt = Format.kasprintf invalid_arg fmt

  let attr_of_ints fg bg st =
    A.fg @@ A.unsafe_color_of_int fg
    |> A.( ++ ) (A.bg @@ A.unsafe_color_of_int bg)
    |> A.( ++ ) (A.st @@ A.unsafe_style_of_int st)
  ;;

  let fg_int i = A.fg @@ A.unsafe_color_of_int i
  let bg_int i = A.bg @@ A.unsafe_color_of_int i

  let print_image_escaped img =
    print_endline "image:";
    img |> Notty.Render.pp_image @@ Format.str_formatter;
    print_endline (Format.flush_str_formatter () |> String.escaped)
  ;;

  let print_image img =
    print_endline "image:";
    img |> Notty.Render.pp_image @@ Format.str_formatter;
    print_endline (Format.flush_str_formatter ())
  ;;

  let print_attr img =
    print_endline "attr:";
    img |> Notty.Render.pp_attr @@ Format.str_formatter;
    print_endline (Format.flush_str_formatter ())
  ;;

  (** Like fold left except we run the first element through init to get the state*)
  let fold_left_pre (f : 'acc -> 'a -> 'acc) (init : 'a -> 'acc) (input : 'a list) =
    match input with
    | [] ->
      invalid_arg "empty list"
    | x :: xs ->
      let state = init x in
      xs |> List.fold_left f state
  ;;
end

module Parser = struct
  open Internal

  type attr_action =
    | Apply of A.t
    | Reset of A.t
    | FullyReset
  let parse_escape_seq =
    let open A in
    let open Angstrom in
    (* let digit = satisfy (function '0' .. '9' -> true | _ -> false) in *)
    let digits = take_while1 (function '0' .. '9' -> true | _ -> false) in
    (* let color_code = digits >>| int_of_string in *)
    let param = digits <* option ' ' (char ';') in
    let params = many (param >>| int_of_string) in
    let escape_sequence = char '\027' *> char '[' *> params <* char 'm' in
    let attr_of_params = function
      | [] ->
        Apply empty
      | 0 :: _ ->
        FullyReset
      | 1 :: _ ->
        Apply (st bold)
      | 2 :: _ ->
        Apply (st dim)
      | 3 :: _ ->
        Apply (st italic)
      | 4 :: _ ->
        Apply (st underline)
      | 5 :: _ ->
        Apply (st blink)
      | 7 :: _ ->
        Apply (st reverse)
      | 8 :: _ ->
        Apply (st hidden)
      | 9 :: _ ->
        Apply (st strike)
      | 21 :: _ ->
        Reset (st bold) (* Double underline or bold off *)
      | 22 :: _ ->
        Reset (st bold ++ st dim) (* Normal intensity - reset bold and dim *)
      | 23 :: _ ->
        Reset (st italic) (* Reset italic *)
      | 24 :: _ ->
        Reset (st underline ) (* Reset underline *)
      | 25 :: _ ->
        Reset (st blink) (* Reset blink *)
      | 27 :: _ ->
        Reset (st reverse) (* Reset reverse *)
      | 28 :: _ ->
        Reset (st hidden) (* Reset hidden *)
      | 29 :: _ ->
        Reset (st strike) (* Reset strikethrough *)
      | 30 :: _ ->
        Apply (fg black)
      | 31 :: _ ->
        Apply (fg red)
      | 32 :: _ ->
        Apply (fg green)
      | 33 :: _ ->
        Apply (fg yellow)
      | 34 :: _ ->
        Apply (fg blue)
      | 35 :: _ ->
        Apply (fg magenta)
      | 36 :: _ ->
        Apply (fg cyan)
      | 37 :: _ ->
        Apply (fg white)
      | 38 :: 5 :: color :: _ ->
        Apply (fg (unsafe_color_of_int (0x01000000 lor color)))
      | 38 :: 2 :: r :: g :: b :: _ ->
        Apply (fg (rgb_888 ~r ~g ~b))
      | 39 :: _ ->
        Reset (fg no_color) (* Default foreground color *)
      | 40 :: _ ->
        Apply (bg black)
      | 41 :: _ ->
        Apply (bg red)
      | 42 :: _ ->
        Apply (bg green)
      | 43 :: _ ->
        Apply (bg yellow)
      | 44 :: _ ->
        Apply (bg blue)
      | 45 :: _ ->
        Apply (bg magenta)
      | 46 :: _ ->
        Apply (bg cyan)
      | 47 :: _ ->
        Apply (bg white)
      | 48 :: 5 :: color :: _ ->
        Apply (bg (unsafe_color_of_int (0x01000000 lor color)))
      | 48 :: 2 :: r :: g :: b :: _ ->
        Apply (bg (rgb_888 ~r ~g ~b))
      | 49 :: _ ->
        Reset (bg no_color) (* Default background color *)
      | 90 :: _ ->
        Apply (fg lightblack) (* Bright black (gray) *)
      | 91 :: _ ->
        Apply (fg lightred)
      | 92 :: _ ->
        Apply (fg lightgreen)
      | 93 :: _ ->
        Apply (fg lightyellow)
      | 94 :: _ ->
        Apply (fg lightblue)
      | 95 :: _ ->
        Apply (fg lightmagenta)
      | 96 :: _ ->
        Apply (fg lightcyan)
      | 97 :: _ ->
        Apply (fg lightwhite)
      | 100 :: _ ->
        Apply (bg lightblack)
      | 101 :: _ ->
        Apply (bg lightred)
      | 102 :: _ ->
        Apply (bg lightgreen)
      | 103 :: _ ->
        Apply (bg lightyellow)
      | 104 :: _ ->
        Apply (bg lightblue)
      | 105 :: _ ->
        Apply (bg lightmagenta)
      | 106 :: _ ->
        Apply (bg lightcyan)
      | 107 :: _ ->
        Apply (bg lightwhite)
      | _ ->
        Apply empty
    in
    escape_sequence >>| attr_of_params
  ;;

  let%expect_test "escape_parser" =
    let test_str = "\027[32m" in
    let res =
      Angstrom.parse_string ~consume:All parse_escape_seq test_str |> Result.get_ok
    in
    (match res with
     | Apply attr ->
       print_attr attr
     | Reset _ ->
       print_endline "Reset attribute"
     | FullyReset ->
       print_endline "Fully reset attribute");
    
    [%expect
      {|
    attr:
    [0m<[0;32mATTR[0m[K[0m>[0m |}]
  ;;

  let parse_ansi_escape_codes (input : string) =
    let attr_state = ref A.empty in
    let open Angstrom in
    let attr = parse_escape_seq in
    let substring = take_while (fun c -> c <> '\027') in
    let pair =
      attr >>= fun action ->
      substring >>= fun s ->
      (match action with
       | Apply a ->
         attr_state := A.( ++ ) !attr_state a
       | Reset a ->
         attr_state := A.( -- ) !attr_state a
       | FullyReset ->
         attr_state := A.empty);
      return (!attr_state, s)
    in
    (* if we don't start on an escape we can match one here*)
    let prefix = option "" substring >>| fun s -> A.empty, s in
    let pairs =
      prefix >>= fun prefix ->
      many pair >>= fun pairs ->
      if prefix |> snd == "" then return pairs else prefix :: pairs |> return
    in
    parse_string ~consume:Prefix pairs input
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
      Parsed 6 segments:
      attr:
      \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
      Text: ""
      attr:
      \e[0m<\e[0;4mATTR\e[0m\e[K\e[0m>\e[0m
      Text: ""
      attr:
      \e[0m<\e[0;31;4mATTR\e[0m\e[K\e[0m>\e[0m
      Text: "\"success\""
      attr:
      \e[0m<\e[0;32;4mATTR\e[0m\e[K\e[0m>\e[0m
      Text: "None"
      attr:
      \e[0m<\e[0mATTR\e[0m\e[K\e[0m>\e[0m
      Text: ""
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
    let no_color = colored -- fg no_color in
    Internal.print_attr no_color;
    [%expect
      {|
      attr:
      \e[0m<\e[0;1;3;4mATTR\e[0m\e[K\e[0m>\e[0m
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
end

(** Converts a string with ansi escape codes to a notty image.
    It parses the escape codes and then creates notty images from that by applying the styles*)
let ansi_string_to_image ?(extra_attr = A.empty) str =
  let str =
    let buffer = Buffer.create (String.length str) in
    let last_char = ref '\000' in
    String.iter
      (fun c ->
         match c with
         | '\r' ->
           last_char := '\r'
         | '\n' when !last_char <> '\r' ->
           Buffer.add_char buffer '\n'
         | '\n' ->
           Buffer.add_char buffer '\n';
           last_char := '\000'
         | '\t' ->
           Buffer.add_string buffer "    "
         | '\x7F' ->
           Buffer.add_string buffer "    "
         | '\x0C' ->
           Buffer.add_string buffer "↡"
         | _ ->
           Buffer.add_char buffer c)
      str;
    Buffer.contents buffer
  in
  match Parser.parse_ansi_escape_codes str with
  | Error a ->
    Printf.printf "restut: %s" a;
    Error a
  | Ok coded_strs ->
    let locate_newlines codes =
      codes
      |> List.concat_map (fun (attr, str) ->
        str
        |> String.split_on_char '\n'
        |> List.map (fun x -> `Image (I.string A.(attr ++ extra_attr) x))
        |> Base.List.intersperse ~sep:`Newline)
    in
    let newline_seperated = locate_newlines coded_strs in
    let lines =
      let open I in
      newline_seperated
      |> Base.List.fold ~init:([], I.empty) ~f:(fun (images, image) x ->
        match x with
        | `Newline ->
          image :: images, I.empty
        | `Image nextImage ->
          images, image <|> nextImage)
      |> fst
      |> Base.List.reduce ~f:(fun bottom top -> top <-> bottom)
      |> Option.value ~default:I.empty
    in
    Ok lines
;;

(** Same as ansi_string_to_image, but can throw if a parsing error occurs. I have not seen it fail, should be safe. *)
let colored_string ?extra_attr ?(max_length = 50000) s =
  let truncated_s =
    if String.length s > max_length
    then String.sub s 0 max_length ^ "\n...truncated"
    else s
  in
  truncated_s |> ansi_string_to_image ?extra_attr |> Result.get_ok
;;
