open Notty
open Angstrom

type op = Buffer.t -> unit

(* let ( & ) op1 op2 buf =
   op1 buf;
   op2 buf

   let ( <| ), ( <. ), ( <! ) = Buffer.(add_string, add_char, add_decimal) *)
let invalid_arg fmt = Format.kasprintf invalid_arg fmt
let sts = [ ";1"; ";3"; ";4"; ";5"; ";7" ]

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

let parse_escape_seq =
  let open A in
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
  let res = parse_string ~consume:All parse_escape_seq test_str |> Result.get_ok in
  print_attr res;
  [%expect {|
    attr:
    [0m<[0;32mATTR[0m[K[0m>[0m |}]
;;

let parse_ansi_escape_codes (input : string) =
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

(** Like fold left except we run the first element through init to get the state*)
let fold_left_pre (f : 'acc -> 'a -> 'acc) (init : 'a -> 'acc) (input : 'a list) =
  match input with
  | [] ->
    invalid_arg "empty list"
  | x :: xs ->
    let state = init x in
    xs |> List.fold_left f state
;;

let string_to_image ?(extra_attr = A.empty) str =
  match parse_ansi_escape_codes str with
  | Error a ->
    Printf.printf "restut: %s" a;
    Error a
  | Ok coded_strs ->
    (* print_endline "parsed"; *)
    let locate_newlines codes =
      codes
      |> List.concat_map (fun (attr, str) ->
        (* print_attr attr; *)
        (* print_endline str; *)
        str
        |> String.split_on_char '\n'
        |> List.map (fun x -> `Image (I.string A.(attr ++ extra_attr) x))
        |> Base.List.intersperse ~sep:`Newline)
    in
    let newline_seperated = locate_newlines coded_strs in
    (* Printf.printf "len:%d" (List.length newline_seperated); *)
    let lines =
      let open I in
      (* newline_seperated
         |> List.iter (fun x -> match x with `Imarge i -> print_image i | _ -> ()); *)
      newline_seperated
      |> Base.List.fold ~init:([], I.empty) ~f:(fun (images, image) x ->
        match x with
        | `Newline ->
          image :: images, I.empty
        | `Image nextImage ->
          images, image <|> nextImage)
      |> fst
      (* |> List.map (fun x ->
         x |> print_image;
         x) *)
      |> Base.List.reduce ~f:(fun bottom top -> top <-> bottom)
      |> Option.value ~default:I.empty
    in
    let image =
      lines
      (* |> fold_left_pre
         (fun image (attr, str) ->
         let parts = str |> String.split_on_char '\n' in
         let nextImage =
         parts
         |> fold_left_pre
         (fun image str -> I.( <-> ) image (I.string attr str))
         (I.string attr)
         in
         I.( <|> ) image nextImage)
         (fun (attr, str) -> I.string attr str) *)
    in
    Ok image
;;

let escaped_string ?(attr = A.empty) str =
  let control_character_index str i =
    let len = String.length str in
    let i = ref i in
    while
      let i = !i in
      i < len && str.[i] >= ' '
    do
      incr i
    done;
    if !i = len then raise Not_found;
    !i
  in
  let rec split str i =
    match control_character_index str i with
    | j ->
      let img = I.string attr (String.sub str i (j - i)) in
      img :: split str (j + 1)
    | exception Not_found ->
      [ I.string attr (if i = 0 then str else String.sub str i (String.length str - i)) ]
  in
  I.vcat (split str 0)
;;

(* let colored_string s =
   s |> parse_ansi_escape_codes
   |> List.map (fun (x, str) -> escaped_string ~attr:x str)
   |> I.vcat *)
let colored_string ?extra_attr s = s |> string_to_image ?extra_attr |> Result.get_ok
