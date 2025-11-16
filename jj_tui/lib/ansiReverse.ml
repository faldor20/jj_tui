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

  (** Prints the attribute in a human readable format.
  Also replaces the escape character with \e.
  This means the output can be copy pasted into a terminal to test.
  like: `echo -e "attr: \e[0;31mTEXT\e[0m"` *)
  let print_attr img =
    print_endline "attr:";
    img |> Notty.Render.pp_attr @@ Format.str_formatter;
    print_endline
      (Format.flush_str_formatter () |> Str.global_replace (Str.regexp "\027") "\\e")
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
    (* SGR (Select Graphic Rendition) sequence: ESC [ params m *)
    let sgr_sequence =
      char '\027' *> char '[' *> params <* char 'm' >>| fun ps -> `Sgr ps
    in
    (* Generic CSI sequence to consume and ignore non-SGR controls like K, J, etc.
       Grammar (simplified): ESC [ <params/intermediates> <final>
       where final is any byte in 0x40..0x7E *)
    let is_final c =
      let code = Char.code c in
      code >= 0x40 && code <= 0x7E
    in
    let non_sgr_csi =
      char '\027'
      *> char '['
      *> take_while (fun c -> not (is_final c))
      *> satisfy is_final
      *> return `Skip
    in
    let escape_any = choice [ sgr_sequence; non_sgr_csi ] in
    let full_seq = many1 escape_any in
    let attr_actions_of_params params =
      let rec loop acc = function
        | [] ->
          List.rev acc
        | 0 :: rest ->
          loop (FullyReset :: acc) rest
        | 1 :: rest ->
          loop (Apply (st bold) :: acc) rest
        | 2 :: rest ->
          loop (Apply (st dim) :: acc) rest
        | 3 :: rest ->
          loop (Apply (st italic) :: acc) rest
        | 4 :: rest ->
          loop (Apply (st underline) :: acc) rest
        | 5 :: rest ->
          loop (Apply (st blink) :: acc) rest
        | 7 :: rest ->
          loop (Apply (st reverse) :: acc) rest
        | 8 :: rest ->
          loop (Apply (st hidden) :: acc) rest
        | 9 :: rest ->
          loop (Apply (st strike) :: acc) rest
        | 21 :: rest ->
          loop (Reset (st bold) :: acc) rest
        | 22 :: rest ->
          loop (Reset (st bold ++ st dim) :: acc) rest
        | 23 :: rest ->
          loop (Reset (st italic) :: acc) rest
        | 24 :: rest ->
          loop (Reset (st underline) :: acc) rest
        | 25 :: rest ->
          loop (Reset (st blink) :: acc) rest
        | 27 :: rest ->
          loop (Reset (st reverse) :: acc) rest
        | 28 :: rest ->
          loop (Reset (st hidden) :: acc) rest
        | 29 :: rest ->
          loop (Reset (st strike) :: acc) rest
        | 30 :: rest ->
          loop (Apply (fg black) :: acc) rest
        | 31 :: rest ->
          loop (Apply (fg red) :: acc) rest
        | 32 :: rest ->
          loop (Apply (fg green) :: acc) rest
        | 33 :: rest ->
          loop (Apply (fg yellow) :: acc) rest
        | 34 :: rest ->
          loop (Apply (fg blue) :: acc) rest
        | 35 :: rest ->
          loop (Apply (fg magenta) :: acc) rest
        | 36 :: rest ->
          loop (Apply (fg cyan) :: acc) rest
        | 37 :: rest ->
          loop (Apply (fg white) :: acc) rest
        | 38 :: 5 :: color :: rest ->
          loop (Apply (fg (unsafe_color_of_int (0x01000000 lor color))) :: acc) rest
        | 38 :: 2 :: r :: g :: b :: rest ->
          loop (Apply (fg (rgb_888 ~r ~g ~b)) :: acc) rest
        | 39 :: rest ->
          loop (Reset (fg color_reset) :: acc) rest
        | 40 :: rest ->
          loop (Apply (bg black) :: acc) rest
        | 41 :: rest ->
          loop (Apply (bg red) :: acc) rest
        | 42 :: rest ->
          loop (Apply (bg green) :: acc) rest
        | 43 :: rest ->
          loop (Apply (bg yellow) :: acc) rest
        | 44 :: rest ->
          loop (Apply (bg blue) :: acc) rest
        | 45 :: rest ->
          loop (Apply (bg magenta) :: acc) rest
        | 46 :: rest ->
          loop (Apply (bg cyan) :: acc) rest
        | 47 :: rest ->
          loop (Apply (bg white) :: acc) rest
        | 48 :: 5 :: color :: rest ->
          loop (Apply (bg (unsafe_color_of_int (0x01000000 lor color))) :: acc) rest
        | 48 :: 2 :: r :: g :: b :: rest ->
          loop (Apply (bg (rgb_888 ~r ~g ~b)) :: acc) rest
        | 49 :: rest ->
          loop (Reset (bg color_reset) :: acc) rest
        | 90 :: rest ->
          loop (Apply (fg lightblack) :: acc) rest
        | 91 :: rest ->
          loop (Apply (fg lightred) :: acc) rest
        | 92 :: rest ->
          loop (Apply (fg lightgreen) :: acc) rest
        | 93 :: rest ->
          loop (Apply (fg lightyellow) :: acc) rest
        | 94 :: rest ->
          loop (Apply (fg lightblue) :: acc) rest
        | 95 :: rest ->
          loop (Apply (fg lightmagenta) :: acc) rest
        | 96 :: rest ->
          loop (Apply (fg lightcyan) :: acc) rest
        | 97 :: rest ->
          loop (Apply (fg lightwhite) :: acc) rest
        | 100 :: rest ->
          loop (Apply (bg lightblack) :: acc) rest
        | 101 :: rest ->
          loop (Apply (bg lightred) :: acc) rest
        | 102 :: rest ->
          loop (Apply (bg lightgreen) :: acc) rest
        | 103 :: rest ->
          loop (Apply (bg lightyellow) :: acc) rest
        | 104 :: rest ->
          loop (Apply (bg lightblue) :: acc) rest
        | 105 :: rest ->
          loop (Apply (bg lightmagenta) :: acc) rest
        | 106 :: rest ->
          loop (Apply (bg lightcyan) :: acc) rest
        | 107 :: rest ->
          loop (Apply (bg lightwhite) :: acc) rest
        | _ :: rest ->
          (* Unknown or unsupported parameter; skip it to avoid stalling *)
          loop acc rest
      in
      loop [] params
    in
    full_seq >>| fun seqs ->
    seqs
    |> List.concat_map (function `Sgr ps -> attr_actions_of_params ps | `Skip -> [])
  ;;

  let print_escape_seq seq =
    print_endline "escape sequence:";
    seq
    |> List.iter (fun action ->
      match action with
      | Apply attr ->
        print_attr attr
      | Reset attr ->
        print_endline "Reset attribute";
        print_attr attr
      | FullyReset ->
        print_endline "Fully reset attribute")
  ;;

  let parse_ansi_escape_codes (input : string) =
    let attr_state = ref A.empty in
    let open Angstrom in
    let attr = parse_escape_seq in
    let substring = take_while (fun c -> c <> '\027') in
    let pair =
      attr >>= fun actions ->
      substring >>= fun s ->
      actions
      |> List.iter (fun action ->
        match action with
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
  (* print_endline ("pre parse str: " ^ str); *)
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
        |> List.map (fun x ->
          let attrs = A.(attr ++ extra_attr) in
          let img = I.string attrs x in
          (* Internal.print_attr attrs;
          print_endline (("str: " ^ x)|> String.escaped);
          Internal.print_image_escaped img; *)
          `Image img)
        |> Base.List.intersperse ~sep:`Newline)
    in
    let newline_seperated = locate_newlines coded_strs in
    let lines =
      let open I in
      let images, last_image =
        newline_seperated
        |> Base.List.fold ~init:([], I.empty) ~f:(fun (images, image) x ->
          match x with
          | `Newline ->
            image :: images, I.empty
          | `Image nextImage ->
            (* print_endline "nextImage:";
          Internal.print_image_escaped nextImage; *)
            images, image <|> nextImage)
      in
      last_image :: images
      |> Base.List.reduce ~f:(fun bottom top ->
        (* print_endline "bottom:";
        Internal.print_image_escaped bottom;
        print_endline "top:";
        Internal.print_image_escaped top; *)
        top <-> bottom)
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
