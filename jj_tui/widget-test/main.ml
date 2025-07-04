open Lwd_infix
open Nottui
open Notty
open Jj_tui.Util
(*
   thoughts on my layout
   1. I want my box to be at most the size of my content
   2. I want my box to shrink untill some fixed limit
*)

let pString s = W.string s |> Lwd.pure

let test_input=
  let inp_var =("hi there",5)|>Lwd.var in
  let inp_text= inp_var|>Lwd.get in

  W.edit_field  inp_text  ~on_change:(fun x->Lwd.set inp_var x) ~on_submit:(fun x->()) 
let w_0 =
  W.hbox
    [
      Ui.border ~thick:2 ~style:Ui.Border.unicode ~label_top:"top" (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;
      Ui.border ~thick:0 ~pad_w:2 ~pad_h:1 ~style:Ui.Border.unicode_double ~label_bottom:"bottom" (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;
      Ui.border ~thick:1 ~style:Ui.Border.unicode_rounded ~label_top:"top" ~label_bottom:"bottom" (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;

      Ui.border ~focus_attr: (A.fg A.red) ~focus_style:Ui.Border.unicode_double ~thick: 2 ~pad_w:2 ~pad_h:1 ~style:Ui.Border.unicode (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;

      W.Box.box ~pad_w:2 ~pad_h:1 (Ui.vcat [W.string "hi this is a ui element with  an\n old style border box"; W.string "hi"]|>Lwd.pure);
      (* pString " |" *)
    (* ; (let og = *)
         (* Ui.vcat *)
           (* [ *)
             (* W.string "123456789000000000000000000000000000000000000000000000000000end" *)
           (* ; W.string "123456789000000000000000000000000000000000000000000000000000end" *)
           (* ] *)
       (* in *)
       (* og *)
       (* |> Lwd.pure *)
       (* |> W.Scroll.area *)
       (* |> W.Box.box *)
       (* |>$ Ui.resize ~sh:1 ~mh:1000 *)
       (* |> W.size_logger) *)
    (* ; pString "| " *)
    test_input|>$ Ui.border  ~focus_attr: (A.fg A.red) ~focus_style:Ui.Border.unicode_double  ~thick:1 ~pad_w:1 ~pad_h:1 ~style:Ui.Border.unicode
    ]
;;

let w_1 =
  W.vbox
    [
      W.hbox
        [
          (let og =
             W.string "123456789000000000000000000000000000000000000000000000000000end"
           in
           og |> Lwd.pure |> W.Scroll.area |> W.Box.box
           (* |>$ Ui.resize ~pad:W.neutral_grav ~crop:W.neutral_grav *))
        ; "shrinkable" |> pString |>$ Ui.resize ~sw:1
        ]
    ]
;;

let w_2 =
  let focus2 = Focus.make () in
  let focus1 = Focus.make () in
  let output =
    W.vbox
      [
        W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og |> Lwd.pure |> W.Scroll.v_area |> W.Box.focusable ~focus:focus1
             (* |>$ Ui.resize ~pad:W.neutral_grav ~crop:W.neutral_grav *))
          ; "shrinkable" |> pString |>$ Ui.resize ~sw:1
          ]
      ; pString "demonstrates stretching to max without any other objects"
      ; W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og
             |> Lwd.pure
             |> W.Scroll.v_area
             |> W.Box.focusable
             |>$ Ui.resize ~pad:Gravity.default ~crop:Gravity.default)
          ]
      ; pString "Same as above but centered"
      ; W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og
             |> Lwd.pure
             |> W.Scroll.area
             |> W.Box.focusable
             |>$ Ui.resize ~pad:W.neutral_grav ~crop:W.neutral_grav)
          ]
      ; W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og
             |> Lwd.pure
             |> W.Scroll.area
             |> W.Box.focusable
             |>$ Ui.resize ~pad:Gravity.default ~crop:Gravity.default)
          ; "shrinkable" |> pString |>$ Ui.resize ~sw:1
          ]
      ; W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og
             |> Lwd.pure
             |> W.Scroll.area
             |> W.Box.focusable
             |>$ Ui.resize ~pad:Gravity.default ~crop:Gravity.default)
          ]
      ; W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og
             |> Lwd.pure
             |> W.Scroll.area
             |> W.Box.focusable
             |> W.size_logger
             |>$ Ui.resize ~pad:Gravity.default ~crop:Gravity.default)
          ]
      ; W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og |> Lwd.pure |> W.Scroll.area)
          ]
      ]
    |>$ Ui.keyboard_area (function
      | `ASCII 'n', _ ->
        Focus.request focus2;
        `Handled
      | `ASCII 'p', _ ->
        Focus.request focus1;
        `Handled
      | _ ->
        `Unhandled)
  in
  Focus.request focus1;
  output
;;

let renderer = Renderer.make ()
let navMode ui = ui
(*  (Focus.status outerFocus)|>$$ (fun ui focus  ->ui|>Ui.keyboard_area (function *)
(* | `Arrow key, mods -> *)
(* let dir : [ `Down | `Left | `Right | `Up ] :> *)
(* [ `Down | `Left | `Right | `Up | `Next | `Prev ] *)
(* = *)
(* key *)
(* in *)
(* Renderer.dispatch_key renderer (`Focus dir, mods) *)
(* | _ -> *)
(* `Unhandled) *)

let is_focused_widget () =
  let focus = Focus.make () in
  let$ focus = focus |> Focus.status in
  W.string (if focus |> Focus.has_focus then "focused " else "not_focused")
  |> Ui.keyboard_area ~focus (fun x -> `Unhandled)
;;

let is_focused_widget2 ~focus =
  let$ focus = focus |> Focus.status in
  W.string (if focus |> Focus.has_focus then "focused " else "not_focused")
  |> Ui.keyboard_area ~focus (fun x -> `Unhandled)
;;

let w_3 =
  let start = Focus.make () in
  Focus.request start;
  W.vbox
    [
      (*
         W.v_window_stack2
         [
          is_focused_widget ()
        ; W.string "hi this is the first level"
          |> Lwd.pure
          |> W.Scroll.area
          |> W.Box.focusable
        ; W.v_window_stack2
            [
              W.string "hi this is the first level"
              |> Lwd.pure
              |> W.Scroll.area
              |> W.Box.focusable
            ; W.string "hi this is the second level"
              |> Lwd.pure
              |> W.Scroll.area
              |> W.Box.focusable ~focus:start
            ; W.v_window_stack2
                [
                  W.string "hi this is the first level"
                  |> Lwd.pure
                  |> W.Scroll.area
                  |> W.Box.focusable
                ; W.string "hi this is the second level"
                  |> Lwd.pure
                  |> W.Scroll.area
                  |> W.Box.focusable
                ]
            ]
        ]
         ; W.v_window_stack2
         [
          W.string "hi this is the first level"
          |> Lwd.pure
          |> W.Scroll.area
          |> W.Box.focusable
        ; W.string "hi this is the second level"
          |> Lwd.pure
          |> W.Scroll.area
          |> W.Box.focusable
        ]
      *)
      (let attr = A.fg A.blue in
       let elem = Ui.hcat [ W.string ~attr "hi"; W.string "there" ] in
       let elem_sw0 =
         Ui.hcat [ W.string ~attr "hi" |> Ui.resize ~mw:1000 ~sw:1; W.string "there" ]
       in
       let elem_sw1 =
         Ui.hcat
           [
             W.string ~attr "hi" |> Ui.resize ~mw:1000 ~sw:1
           ; W.string "there" |> Ui.resize ~mw:1000 ~sw:2
           ]
       in
       let elem_w =
         Ui.hcat
           [
             W.string ~attr "1234567890abcd" |> Ui.resize ~w:10 ~sw:1 ~mw:1000
           ; W.string "there" |> Ui.resize ~sw:1 ~w:5 ~mw:1000
           ]
       in
       let elem_mw =
         Ui.hcat
           [
             W.string ~attr "1234567890abcd" |> Ui.resize ~w:5 ~sw:1 ~mw:7
           ; W.string "there" |> Ui.resize ~sw:2
           ]
       in
       Ui.vcat [ elem; elem_sw0; elem_sw1; elem_w; elem_mw ]
       |> Ui.resize ~w:15 ~sw:0
       |> Lwd.pure
       |> W.Box.box ~pad_w:0 ~pad_h:0)
    ; (let output = Lwd.var "none" in
       (*A button that responds to the enter keypress*)
       let button ?(focus = Focus.make ()) name =
         let$ focus = focus |> Focus.status in
         W.string name
         |> Ui.keyboard_area ~focus (function
           | `Enter, _ ->
             output $= "inner";
             `Handled
           | _ ->
             `Unhandled)
       in
       let outerFocus = Focus.make () in
       (*We wrap the button in some more UI*)
       let$ outer = W.vbox [ button "I'm a button"; Lwd.get output |>$ W.string ]
       and$ focus = Focus.status outerFocus in
       outer
       (*We also give the outer UI respond the "Enter" keypress*)
       |> Ui.keyboard_area ~focus (function
         | `Enter, _ ->
           output $= "outer";
           `Handled
         | _ ->
           `Unhandled))
    ]
;;

Ui.keyboard_area

let w_5 =
  let focus = Focus.make () in
  Focus.request focus;
  W.Wip.v_window_stack
    ~focus
    [
      is_focused_widget2
    ; is_focused_widget2
    ; W.Wip.h_window_stack
        [
          is_focused_widget2
        ; is_focused_widget2
        ; W.Wip.v_window_stack
            [
              is_focused_widget2
            ; is_focused_widget2
            ; W.Wip.h_window_stack
                [
                  is_focused_widget2
                ; is_focused_widget2
                ; W.Wip.v_window_stack [ is_focused_widget2; is_focused_widget2 ]
                ]
            ]
        ]
    ; W.Wip.h_window_stack [ is_focused_widget2 ]
    ]
;;

let w_4 =
  let reaction = Lwd.var true in
  let result = Lwd.var "" in
  W.vbox
    [
      (let$* _ = Lwd.get reaction in
       result $= Lwd.peek result ^ "->outer";
       W.hbox
         [
           W.string "hi this is the first level" |> Lwd.pure
         ; (let$ _ = Lwd.get reaction in
            result $= Lwd.peek result ^ "->inner";
            W.string " hi this is the second level")
         ])
    ; (let$ result = Lwd.get result in
       W.string result)
    ]
  |>$ Ui.keyboard_area (function
    | `Enter, _ ->
      reaction $= false;
      `Handled
    | _ ->
      `Unhandled)
;;

let w_6 =
  W.vbox
    [
      Ui.vcat [ W.string "hi"; W.string "50"; W.string "there"; W.string "mate" ]
      |> Lwd.pure
      |> W.Scroll.v_area
      |>$ Ui.resize ~h:2 ~sh:0
    ; W.h_rule |> Lwd.pure
    ; W.Old.file_select ~on_select:(fun x -> ()) ()
    ; W.unfoldable
        (W.string "click to unfold" |> Lwd.pure)
        (fun () -> W.string "I'm unfolded" |> Lwd.pure)
    ]
;;

let w_7 =
  W.vbox
    [ (*
         W.Wip.h_window_stack2
         [
          Ui.vcat
            [
              W.string "hi"
            ; W.string "50"
            ; W.string "there345678987654356789876545end"
            ; W.string "mate"
            ]
          |> Lwd.pure
          |> W.Scroll.area
          |>$ Ui.resize ~mw:15
        ; W.string "|" |> Lwd.pure
        ]
         ; W.Wip.h_window_stack2
         [
          Ui.vcat
            [
              W.string "hi"
            ; W.string "50"
            ; W.string "there345678987654356789876545end"
            ; W.string "mate"
            ]
          |> Lwd.pure
          |> W.Scroll.v_area
          |>$ Ui.resize ~h:2 ~sh:0
        ; W.string "|" |> Lwd.pure
        ]
         ; W.string "----" |> Lwd.pure
      *) ]
;;

let w_8 =
  let top = Lwd.var "" in
  let bot = Lwd.var "" in
  let last = Lwd.var "" in
  let topfocus = Focus.make () in
  W.vbox
    [
      Lwd.get top
      |>$ W.string
      |>$ Ui.keyboard_area (function
        | `ASCII 'p', _ ->
          top $= Lwd.peek top ^ "p";
          `Handled
        | `ASCII 'c', _ ->
          top $= Lwd.peek top ^ "c";
          `Handled
        | _ ->
          `Unhandled)
      |> W.Box.focusable ~focus:topfocus
    ; Lwd.get bot
      |>$ W.string
      |>$ Ui.keyboard_area (function
        | `ASCII 'p', _ ->
          bot $= Lwd.peek bot ^ "p";
          `Handled
        | `ASCII 'd', _ ->
          bot $= Lwd.peek bot ^ "d";
          `Handled
        | `ASCII 'b', _ ->
          bot $= Lwd.peek bot ^ "b";
          `Handled
        | _ ->
          `Unhandled)
      |> W.Box.focusable
    ; Lwd.get last
      |>$ W.string
      |>$ Ui.keyboard_area (function
        | `ASCII 'p', _ ->
          last $= Lwd.peek last ^ "p";
          `Handled
        | `ASCII 'd', _ ->
          last $= Lwd.peek last ^ "d";
          `Handled
        | `ASCII 'l', _ ->
          last $= Lwd.peek last ^ "l";
          `Handled
        | _ ->
          `Unhandled)
      |> W.Box.focusable
    ]
  |>$ Ui.keyboard_area (function
    | `ASCII 's', _ ->
      Focus.request topfocus;
      `Handled
    | _ ->
      `Unhandled)
;;

(* |> Lwd.observe *)
(* |> Lwd.quick_sample *)
(* |> Ui.pp Format.str_formatter; *)
(* pString (Format.flush_str_formatter())|>W.Scroll.v_area *)

let w_9 =
  let items =
    [ "hi"; "it's"; "meeeeeeeeeeeeeeeeeeeeeeeee" ]
    |> List.map (fun item ->
      W.Lists.
        {
          data = item
        ; id = item |> String.hash
        ; ui = W.Lists.selectable_item (W.string item)
        })
    |> Lwd.pure
  in
  W.vbox
    [
      items
      |> W.Lists.selection_list_custom
           ~on_selection_change:(fun x -> ())
           ~custom_handler:(fun _ key -> match key with _ -> `Unhandled)
      |>$ Ui.resize ~w:10 ~sw:1
      |> W.Box.focusable ~pad_h:0
    ; items
      |> W.Lists.selection_list_custom
           ~on_selection_change:(fun x -> ())
           ~custom_handler:(fun _ key -> match key with _ -> `Unhandled)
      |>$ Ui.resize ~w:10 ~sw:1
      |> W.Box.focusable ~pad_h:0
    ; items
      |> W.Lists.selection_list_custom
           ~on_selection_change:(fun x -> ())
           ~custom_handler:(fun _ key -> match key with _ -> `Unhandled)
      |>$ Ui.resize ~w:10 ~sw:1
      |> W.Box.focusable ~pad_h:0
    ]
;;

let quit = Lwd.var false

let main_ui =
  let test_number = Lwd.var 0 in
  let$* test_num = Lwd.get test_number in
  (match test_num with
   | 0 ->
     w_0
   | 1 ->
     w_1
   | 2 ->
     w_2
   | 3 ->
     w_3
   | 4 ->
     w_4
   | 5 ->
     w_5
   | 6 ->
     w_6
   | 7 ->
     w_7
   | 8 ->
     w_8
   | 9 ->
     w_9
   | _ ->
     W.string "not a test" |> Lwd.pure)
  |>$ Ui.event_filter (function
    | `Key (`ASCII '1', _) ->
      Lwd.set test_number 1;
      `Handled
    | `Key (`ASCII '2', _) ->
      Lwd.set test_number 2;
      `Handled
    | `Key (`ASCII '3', _) ->
      Lwd.set test_number 3;
      `Handled
    | `Key (`ASCII '4', _) ->
      Lwd.set test_number 4;
      `Handled
    | `Key (`ASCII '5', _) ->
      Lwd.set test_number 5;
      `Handled
    | `Key (`ASCII '6', _) ->
      Lwd.set test_number 6;
      `Handled
    | `Key (`ASCII '7', _) ->
      Lwd.set test_number 7;
      `Handled
    | `Key (`ASCII '8', _) ->
      Lwd.set test_number 8;
      `Handled
    | `Key (`ASCII '9', _) ->
      Lwd.set test_number 9;
      `Handled
    | `Key (`ASCII 'q', [ `Ctrl ]) ->
      quit $= true;
      `Handled
    | _ ->
      `Unhandled)
;;

(* w_3|>Lwd.observe|>Lwd.quick_sample|> *)
(* Ui.pp Format.std_formatter ;; *)
Nottui.Ui_loop.run ~renderer ~quit main_ui
