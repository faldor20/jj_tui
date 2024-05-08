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
  [%expect {| |}]
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

let string_to_image str =
  match parse_ansi_escape_codes str with
  | Error a ->
    Printf.printf "restut: %s" a;
    Error a
  | Ok coded_strs ->
    print_endline "parsed";
    let locate_newlines codes =
      codes
      |> List.concat_map (fun (attr, str) ->
        print_attr attr;
        print_endline str;
        str
        |> String.split_on_char '\n'
        |> List.map (fun x -> `Image (I.string attr x))
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
      |> Base.List.reduce_exn ~f:(fun bottom top -> top <-> bottom)
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
let colored_string s = s |> string_to_image

let%expect_test "string_to_image" =
  string_to_image "\027[32mThis is in green %s\027[0m \027[30mThisisnotGreen\027[0m"
  |> Result.get_ok
  |> print_image;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Invalid_argument List.reduce_exn)
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Jj_tui__AnsiReverse.(fun) in file "lib/ansiReverse.ml", line 197, characters 2-88
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  params 32
  params 0
  params 30
  params 0
  len:4 |}]
;;

let%expect_test "hello" =
  let outBuf = Buffer.create 100 in
  let _fmt = Format.formatter_of_buffer outBuf in
  let res =
    parse_ansi_escape_codes
      "\027[32mThis is in green %s\027[0m \027[30mThisisnotGreen\027[0m"
    |> Result.get_ok
  in
  res
  |> List.iter (fun (x, str) ->
    Notty.I.string x str |> Notty.Render.pp_image @@ Format.str_formatter);
  print_endline (Format.flush_str_formatter () |> String.escaped);
  print_endline (Buffer.contents outBuf);
  [%expect
    {|
      params 32
      params 0
      params 30
      params 0
      \027[0m\027[K\027[0;32mThis is in green %s\027[0m\027[0m\027[K\027[0m \027[0m\027[0m\027[K\027[0;30mThisisnotGreen\027[0m\027[0m\027[K\027[0m|}]
;;

let jjtest =
  {|
  @  [1m[38;5;13mm[38;5;8mtxzlotn[39m [38;5;3meli.jambu@gmail.com[39m [38;5;14m2024-05-08 12:19:37[39m [38;5;12mb[38;5;8mb87f772[39m[0m
  │  [1m[38;5;3m(no description set)[39m[0m
|}
;;

let%expect_test "jj_test" =
  jjtest
  |> string_to_image
  |> Result.get_ok
  |> Notty.Render.pp_image @@ Format.str_formatter;
  let res = Format.flush_str_formatter () in
  print_endline "====== input=====";
  print_endline (jjtest |> String.escaped);
  print_endline "====== output escaped=====";
  print_endline (res |> String.escaped);
  print_endline "=====output====";
  print_endline res;
  [%expect
    {|
      params 1
      params 38;5;13
      nfg [0m<[0;95mATTR[0m[K[0m>[0m
      params 38;5;8
      nfg [0m<[0;90mATTR[0m[K[0m>[0m
      params 39
      params 38;5;3
      nfg [0m<[0;33mATTR[0m[K[0m>[0m
      params 39
      params 38;5;14
      nfg [0m<[0;96mATTR[0m[K[0m>[0m
      params 39
      params 38;5;12
      nfg [0m<[0;94mATTR[0m[K[0m>[0m
      params 38;5;8
      nfg [0m<[0;90mATTR[0m[K[0m>[0m
      params 39
      params 0
      params 1
      params 38;5;3
      nfg [0m<[0;33mATTR[0m[K[0m>[0m
      params 39
      params 0
      len:23image:
      \027[0m  \226\148\130  \027[0m\027[K\027[0;33m(no description set)\027[0m
      image:
      \027[0m  @  \027[0;95mm\027[0;90mtxzlotn\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;96m2024-05-08 12:19:37\027[0m \027[0;94mb\027[0m\027[K\027[0;90mb87f772\027[0m
      image:
      \027[0m\027[K\027[0m
      ====== input=====
      \n  @  \027[1m\027[38;5;13mm\027[38;5;8mtxzlotn\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;14m2024-05-08 12:19:37\027[39m \027[38;5;12mb\027[38;5;8mb87f772\027[39m\027[0m\n  \226\148\130  \027[1m\027[38;5;3m(no description set)\027[39m\027[0m\n
      ====== output escaped=====
      \027[0m\027[K\027[0m\n\027[0m  @  \027[0;95mm\027[0;90mtxzlotn\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;96m2024-05-08 12:19:37\027[0m \027[0;94mb\027[0m\027[K\027[0;90mb87f772\027[0m\n\027[0m  \226\148\130  \027[0;33m(no description set)\027[0m\027[K\027[0m                                     \027[0m
      =====output====
      |}]
;;

(* let ansi =
   {
     skip =
       (fun (b : Buffer.t) ->
         let n = int_of_string (Buffer.contents b) in
         Buffer.clear b;
         n);
     newline = (fun _ -> ());
     altscr =
       (fun b ->
         Buffer.clear b;
         String.equal (Buffer.contents b) "\x1b[?1049h");
     cursat =
       (fun b ->
         let h = int_of_string (Buffer.contents b) in
         Buffer.clear b;
         Buffer.clear b;
         (* skip ';' *)
         let w = int_of_string (Buffer.contents b) in
         Buffer.clear b;
         (w - 1, h - 1));
     cubcuf =
       (fun b ->
         let x = int_of_string (Buffer.contents b) in
         Buffer.clear b;
         if Buffer.nth b 0 = 'D' then -x else x);
     cuucud =
       (fun b ->
         let y = int_of_string (Buffer.contents b) in
         Buffer.clear b;
         if Buffer.nth b 0 = 'A' then -y else y);
     cr = (fun _ -> ());
     clreol = (fun _ -> ());
     cursvis =
       (fun b ->
         Buffer.clear b;
         String.equal (Buffer.contents b) "\x1b[34h\x1b[?25h");
     mouse =
       (fun b ->
         Buffer.clear b;
         String.equal (Buffer.contents b) "\x1b[?1000;1002;1005;1015;1006h");
     bpaste =
       (fun b ->
         Buffer.clear b;
         String.equal (Buffer.contents b) "\x1b[?2004h");
     sgr;
   } *)
(*
   let no0 _ = ()
     and no1 _ _ = ()
     and no2 _ _ _ = ()

     let dumb =
       {
         skip = (fun b -> String.length (Buffer.contents b));
         newline = (fun _ -> ());
         altscr = no1;
         cursat = (fun _ -> (0, 0));
         cubcuf = (fun _ -> 0);
         cuucud = (fun _ -> 0);
         cr = no0;
         clreol = no0;
         cursvis = (fun _ -> false);
         sgr = (fun _ -> A.empty);
         mouse = (fun _ -> false);
         bpaste = (fun _ -> false);
       } *)
(*
   let erase cap buf = Buffer.clear buf (* KEEP ETA-LONG. *)

   let cursat0 cap b =
   let w, h = cap.cursat b in
   (max 0 (w - 1), max 0 (h - 1)) *)
