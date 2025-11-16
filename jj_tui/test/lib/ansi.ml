open Jj_tui.AnsiReverse

let%expect_test "jj_test" =
  let jjtest =
    {|
◉  [1m[38;5;5myq[0m[38;5;8mytskyk[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-13 09:34:43[39m [1m[38;5;4mb[0m[38;5;8m432b3c1[39m
│  test reorganise
@  [1m[38;5;13mtw[38;5;8msoqryt[39m [38;5;3meli.jambu@gmail.com[39m [38;5;14m2024-05-13 09:34:43[39m [38;5;12m87[38;5;8md4ffad[39m[0m
│  [1mupdated flakes[0m
◉  [1m[38;5;5mys[0m[38;5;8mzqynxv[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-13 08:22:36[39m [38;5;2mHEAD@git[39m [1m[38;5;4m65[0m[38;5;8md9b7dc[39m
│  opam template
◉  [1m[38;5;5mkr[0m[38;5;8mzvxzyw[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-13 07:53:04[39m [1m[38;5;4m0c[0m[38;5;8mf0a9b8[39m
│  different strat
◉  [1m[38;5;5ml[0m[38;5;8mzrkyqxq[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-12 20:28:46[39m [38;5;5mmaster?? master?? master@git master@origin[39m [1m[38;5;4me3[0m[38;5;8me2ba28[39m
│  remove vendor libs
│ ◉  [1m[38;5;5mq[0m[38;5;8mpqzkuss[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-12 21:07:29[39m [1m[38;5;4m5[0m[38;5;8m1e7fabe[39m
│ │  [38;5;3m(no description set)[39m
│ ◉  [1m[38;5;5mx[0m[38;5;8mpqmtrmp[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-12 20:31:20[39m [1m[38;5;4me5[0m[38;5;8mcaae1c[39m
├─╯  remove old nix file
◉  [1m[38;5;5mzx[0m[38;5;8mpskuop[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-12 00:43:25[39m [1m[38;5;4m3[0m[38;5;8m3771185[39m
│  Update README.md
◌  [38;5;8m(elided revisions)[39m
│ ◉  [1m[38;5;5mn[0m[38;5;8mwxyqxuv[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-11 14:11:37[39m [1m[38;5;4m89[0m[38;5;8m392bc6[39m
├─╯  [38;5;3m(no description set)[39m
◉  [1m[38;5;5mkm[0m[38;5;8mosytmo[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-11 14:11:37[39m [1m[38;5;4m4[0m[38;5;8m1122b29[39m
│  backup opam
◌  [38;5;8m(elided revisions)[39m
│ ◉  [1m[38;5;5mto[0m[38;5;8moppyyl[39m [38;5;3meli.jambu@gmail.com[39m [38;5;6m2024-05-11 03:29:14[39m [1m[38;5;4m6f[0m[38;5;8md850b1[39m
├─╯  test
◉  [1m[38;5;5mzz[0m[38;5;8mzzzzzz[39m [38;5;2mroot()[39m [1m[38;5;4m00[0m[38;5;8m000000[39m
  |}
  in
  jjtest
  |> ansi_string_to_image
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
    ====== input=====
    \n\226\151\137  \027[1m\027[38;5;5myq\027[0m\027[38;5;8mytskyk\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-13 09:34:43\027[39m \027[1m\027[38;5;4mb\027[0m\027[38;5;8m432b3c1\027[39m\n\226\148\130  test reorganise\n@  \027[1m\027[38;5;13mtw\027[38;5;8msoqryt\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;14m2024-05-13 09:34:43\027[39m \027[38;5;12m87\027[38;5;8md4ffad\027[39m\027[0m\n\226\148\130  \027[1mupdated flakes\027[0m\n\226\151\137  \027[1m\027[38;5;5mys\027[0m\027[38;5;8mzqynxv\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-13 08:22:36\027[39m \027[38;5;2mHEAD@git\027[39m \027[1m\027[38;5;4m65\027[0m\027[38;5;8md9b7dc\027[39m\n\226\148\130  opam template\n\226\151\137  \027[1m\027[38;5;5mkr\027[0m\027[38;5;8mzvxzyw\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-13 07:53:04\027[39m \027[1m\027[38;5;4m0c\027[0m\027[38;5;8mf0a9b8\027[39m\n\226\148\130  different strat\n\226\151\137  \027[1m\027[38;5;5ml\027[0m\027[38;5;8mzrkyqxq\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-12 20:28:46\027[39m \027[38;5;5mmaster?? master?? master@git master@origin\027[39m \027[1m\027[38;5;4me3\027[0m\027[38;5;8me2ba28\027[39m\n\226\148\130  remove vendor libs\n\226\148\130 \226\151\137  \027[1m\027[38;5;5mq\027[0m\027[38;5;8mpqzkuss\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-12 21:07:29\027[39m \027[1m\027[38;5;4m5\027[0m\027[38;5;8m1e7fabe\027[39m\n\226\148\130 \226\148\130  \027[38;5;3m(no description set)\027[39m\n\226\148\130 \226\151\137  \027[1m\027[38;5;5mx\027[0m\027[38;5;8mpqmtrmp\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-12 20:31:20\027[39m \027[1m\027[38;5;4me5\027[0m\027[38;5;8mcaae1c\027[39m\n\226\148\156\226\148\128\226\149\175  remove old nix file\n\226\151\137  \027[1m\027[38;5;5mzx\027[0m\027[38;5;8mpskuop\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-12 00:43:25\027[39m \027[1m\027[38;5;4m3\027[0m\027[38;5;8m3771185\027[39m\n\226\148\130  Update README.md\n\226\151\140  \027[38;5;8m(elided revisions)\027[39m\n\226\148\130 \226\151\137  \027[1m\027[38;5;5mn\027[0m\027[38;5;8mwxyqxuv\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-11 14:11:37\027[39m \027[1m\027[38;5;4m89\027[0m\027[38;5;8m392bc6\027[39m\n\226\148\156\226\148\128\226\149\175  \027[38;5;3m(no description set)\027[39m\n\226\151\137  \027[1m\027[38;5;5mkm\027[0m\027[38;5;8mosytmo\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-11 14:11:37\027[39m \027[1m\027[38;5;4m4\027[0m\027[38;5;8m1122b29\027[39m\n\226\148\130  backup opam\n\226\151\140  \027[38;5;8m(elided revisions)\027[39m\n\226\148\130 \226\151\137  \027[1m\027[38;5;5mto\027[0m\027[38;5;8moppyyl\027[39m \027[38;5;3meli.jambu@gmail.com\027[39m \027[38;5;6m2024-05-11 03:29:14\027[39m \027[1m\027[38;5;4m6f\027[0m\027[38;5;8md850b1\027[39m\n\226\148\156\226\148\128\226\149\175  test\n\226\151\137  \027[1m\027[38;5;5mzz\027[0m\027[38;5;8mzzzzzz\027[39m \027[38;5;2mroot()\027[39m \027[1m\027[38;5;4m00\027[0m\027[38;5;8m000000\027[39m\n
    ====== output escaped=====
    \027[0m\027[K\027[0m\n\027[0m\226\151\137  \027[0;35;1myq\027[0;90mytskyk\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-13 09:34:43\027[0m \027[0;34;1mb\027[0m\027[K\027[0;90m432b3c1\027[0m\n\027[0m\027[K\027[0m\226\148\130  test reorganise\027[0m\n\027[0m@  \027[0;95;1mtw\027[0;90;1msoqryt\027[0;1m \027[0;33;1meli.jambu@gmail.com\027[0;1m \027[0;96;1m2024-05-13 09:34:43\027[0;1m \027[0;94;1m87\027[0m\027[K\027[0;90;1md4ffad\027[0m\n\027[0m\226\148\130  \027[0m\027[K\027[0;1mupdated flakes\027[0m\n\027[0m\226\151\137  \027[0;35;1mys\027[0;90mzqynxv\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-13 08:22:36\027[0m \027[0;32mHEAD@git\027[0m \027[0;34;1m65\027[0m\027[K\027[0;90md9b7dc\027[0m\n\027[0m\027[K\027[0m\226\148\130  opam template\027[0m\n\027[0m\226\151\137  \027[0;35;1mkr\027[0;90mzvxzyw\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-13 07:53:04\027[0m \027[0;34;1m0c\027[0m\027[K\027[0;90mf0a9b8\027[0m\n\027[0m\027[K\027[0m\226\148\130  different strat\027[0m\n\027[0m\226\151\137  \027[0;35;1ml\027[0;90mzrkyqxq\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-12 20:28:46\027[0m \027[0m\027[K\027[0;35mmaster?? master?? master@g\027[0m\n\027[0m\027[K\027[0m\226\148\130  remove vendor libs\027[0m\n\027[0m\226\148\130 \226\151\137  \027[0;35;1mq\027[0;90mpqzkuss\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-12 21:07:29\027[0m \027[0;34;1m5\027[0m\027[K\027[0;90m1e7fabe\027[0m\n\027[0m\226\148\130 \226\148\130  \027[0m\027[K\027[0;33m(no description set)\027[0m\n\027[0m\226\148\130 \226\151\137  \027[0;35;1mx\027[0;90mpqmtrmp\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-12 20:31:20\027[0m \027[0;34;1me5\027[0m\027[K\027[0;90mcaae1c\027[0m\n\027[0m\027[K\027[0m\226\148\156\226\148\128\226\149\175  remove old nix file\027[0m\n\027[0m\226\151\137  \027[0;35;1mzx\027[0;90mpskuop\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-12 00:43:25\027[0m \027[0;34;1m3\027[0m\027[K\027[0;90m3771185\027[0m\n\027[0m\027[K\027[0m\226\148\130  Update README.md\027[0m\n\027[0m\226\151\140  \027[0m\027[K\027[0;90m(elided revisions)\027[0m\n\027[0m\226\148\130 \226\151\137  \027[0;35;1mn\027[0;90mwxyqxuv\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-11 14:11:37\027[0m \027[0;34;1m89\027[0m\027[K\027[0;90m392bc6\027[0m\n\027[0m\226\148\156\226\148\128\226\149\175  \027[0m\027[K\027[0;33m(no description set)\027[0m\n\027[0m\226\151\137  \027[0;35;1mkm\027[0;90mosytmo\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-11 14:11:37\027[0m \027[0;34;1m4\027[0m\027[K\027[0;90m1122b29\027[0m\n\027[0m\027[K\027[0m\226\148\130  backup opam\027[0m\n\027[0m\226\151\140  \027[0m\027[K\027[0;90m(elided revisions)\027[0m\n\027[0m\226\148\130 \226\151\137  \027[0;35;1mto\027[0;90moppyyl\027[0m \027[0;33meli.jambu@gmail.com\027[0m \027[0;36m2024-05-11 03:29:14\027[0m \027[0;34;1m6f\027[0m\027[K\027[0;90md850b1\027[0m\n\027[0m\027[K\027[0m\226\148\156\226\148\128\226\149\175  test\027[0m\n\027[0m\226\151\137  \027[0;35;1mzz\027[0;90mzzzzzz\027[0m \027[0;32mroot()\027[0m \027[0;34;1m00\027[0m\027[K\027[0;90m000000\027[0m\n\027[0m  \027[0m\027[K\027[0m                                                                            \027[0m
    =====output====
    [0m[K[0m
    [0m◉  [0;35;1myq[0;90mytskyk[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-13 09:34:43[0m [0;34;1mb[0m[K[0;90m432b3c1[0m
    [0m[K[0m│  test reorganise[0m
    [0m@  [0;95;1mtw[0;90;1msoqryt[0;1m [0;33;1meli.jambu@gmail.com[0;1m [0;96;1m2024-05-13 09:34:43[0;1m [0;94;1m87[0m[K[0;90;1md4ffad[0m
    [0m│  [0m[K[0;1mupdated flakes[0m
    [0m◉  [0;35;1mys[0;90mzqynxv[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-13 08:22:36[0m [0;32mHEAD@git[0m [0;34;1m65[0m[K[0;90md9b7dc[0m
    [0m[K[0m│  opam template[0m
    [0m◉  [0;35;1mkr[0;90mzvxzyw[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-13 07:53:04[0m [0;34;1m0c[0m[K[0;90mf0a9b8[0m
    [0m[K[0m│  different strat[0m
    [0m◉  [0;35;1ml[0;90mzrkyqxq[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-12 20:28:46[0m [0m[K[0;35mmaster?? master?? master@g[0m
    [0m[K[0m│  remove vendor libs[0m
    [0m│ ◉  [0;35;1mq[0;90mpqzkuss[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-12 21:07:29[0m [0;34;1m5[0m[K[0;90m1e7fabe[0m
    [0m│ │  [0m[K[0;33m(no description set)[0m
    [0m│ ◉  [0;35;1mx[0;90mpqmtrmp[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-12 20:31:20[0m [0;34;1me5[0m[K[0;90mcaae1c[0m
    [0m[K[0m├─╯  remove old nix file[0m
    [0m◉  [0;35;1mzx[0;90mpskuop[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-12 00:43:25[0m [0;34;1m3[0m[K[0;90m3771185[0m
    [0m[K[0m│  Update README.md[0m
    [0m◌  [0m[K[0;90m(elided revisions)[0m
    [0m│ ◉  [0;35;1mn[0;90mwxyqxuv[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-11 14:11:37[0m [0;34;1m89[0m[K[0;90m392bc6[0m
    [0m├─╯  [0m[K[0;33m(no description set)[0m
    [0m◉  [0;35;1mkm[0;90mosytmo[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-11 14:11:37[0m [0;34;1m4[0m[K[0;90m1122b29[0m
    [0m[K[0m│  backup opam[0m
    [0m◌  [0m[K[0;90m(elided revisions)[0m
    [0m│ ◉  [0;35;1mto[0;90moppyyl[0m [0;33meli.jambu@gmail.com[0m [0;36m2024-05-11 03:29:14[0m [0;34;1m6f[0m[K[0;90md850b1[0m
    [0m[K[0m├─╯  test[0m
    [0m◉  [0;35;1mzz[0;90mzzzzzz[0m [0;32mroot()[0m [0;34;1m00[0m[K[0;90m000000[0m
    [0m  [0m[K[0m                                                                            [0m
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
(**gets the description of the current and previous change. Useful when squashing*)
