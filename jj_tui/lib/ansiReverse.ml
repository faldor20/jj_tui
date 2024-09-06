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
        empty
      | 0 :: _ ->
        empty
      | 1 :: _ ->
        st bold
      | 2 :: _ ->
        st italic
      | 4 :: _ ->
        st underline
      | 5 :: _ ->
        st blink
      | 7 :: _ ->
        st reverse
      | 30 :: _ ->
        fg black
      | 31 :: _ ->
        fg red
      | 32 :: _ ->
        fg green
      | 33 :: _ ->
        fg yellow
      | 34 :: _ ->
        fg blue
      | 35 :: _ ->
        fg magenta
      | 36 :: _ ->
        fg cyan
      | 37 :: _ ->
        fg white
      | 38 :: 5 :: color :: _ ->
        fg (unsafe_color_of_int (0x01000000 lor color))
      | 40 :: _ ->
        bg black
      | 41 :: _ ->
        bg red
      | 42 :: _ ->
        bg green
      | 43 :: _ ->
        bg yellow
      | 44 :: _ ->
        bg blue
      | 45 :: _ ->
        bg magenta
      | 46 :: _ ->
        bg cyan
      | 47 :: _ ->
        bg white
      | 48 :: 5 :: color :: _ ->
        bg (unsafe_color_of_int (0x01000000 lor color))
      | _ ->
        empty
    in
    escape_sequence >>| attr_of_params
  ;;

  let%expect_test "escape_parser" =
    let test_str = "\027[32m" in
    let res =
      Angstrom.parse_string ~consume:All parse_escape_seq test_str |> Result.get_ok
    in
    print_attr res;
    [%expect {|
    attr:
    [0m<[0;32mATTR[0m[K[0m>[0m |}]
  ;;

  let parse_ansi_escape_codes (input : string) =
    let open Angstrom in
    let attr = parse_escape_seq in
    let substring = take_while (fun c -> c <> '\027') in
    let pair =
      attr >>= fun a ->
      substring >>= fun s -> return (a, s)
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
end

(** Converts a string with ansi escape codes to a notty image.
    It parses the escape codes and then creates notty images from that by applying the styles*)
let ansi_string_to_image ?(extra_attr = A.empty) str =
  let str =
    (* replace any carrriage returns becasue notty doesn't know what to do with them*)
    Base.String.Search_pattern.replace_all
      (Base.String.Search_pattern.create "\r\n")
      ~in_:str
      ~with_:"\n"
    |> Base.String.Search_pattern.replace_all
         (Base.String.Search_pattern.create "\r")
         ~with_:"\n"
    (*tabs cause issues too*)
    |> Base.String.Search_pattern.replace_all
         (Base.String.Search_pattern.create "\t")
         ~with_:"    "
     (*delete control char*)
    |> Base.String.Search_pattern.replace_all
         (Base.String.Search_pattern.create "\u{7f}")
         ~with_:"    "
    (*replace form feed with a symbol: https://codepoints.net/U+000C?lang=en*)
    |> Base.String.Search_pattern.replace_all
         (Base.String.Search_pattern.create "")
         ~with_:"↡"
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
let colored_string ?extra_attr s = s |> ansi_string_to_image ?extra_attr |> Result.get_ok
