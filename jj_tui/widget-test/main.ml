open Lwd_infix
open Nottui
open Notty
open Jj_tui.Util
module W = Nottui_widgets
module Wd = Jj_tui.Widgets
(*
   thoughts on my layout
   1. I want my box to be at most the size of my content
   2. I want my box to shrink untill some fixed limit
*)

let pString s = W.string s |> Lwd.pure

let w_0 =
  W.vbox
    [
      W.hbox
        [
          (let og =
             W.string "123456789000000000000000000000000000000000000000000000000000end"
           in
           let focus = Focus.make () in
           og |> Lwd.pure |> W.scroll_area |> Wd.border_box ~scaling:(`Shrinkable (15, 1))
           (* |>$ Ui.resize ~pad:Wd.neutral_grav ~crop:Wd.neutral_grav *));
          "shrinkable" |> pString |>$ Ui.resize ~sw:1;
        ];
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
           og |> Lwd.pure |> W.scroll_area |> Wd.border_box ~scaling:(`Shrinkable (15, 1))
           (* |>$ Ui.resize ~pad:Wd.neutral_grav ~crop:Wd.neutral_grav *));
          "shrinkable" |> pString |>$ Ui.resize ~sw:1;
        ];
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
             og
             |> Lwd.pure
             |> W.scroll_area
             |> Wd.border_box_focusable ~focus:focus1 ~scaling:(`Shrinkable (15, 1))
             (* |>$ Ui.resize ~pad:Wd.neutral_grav ~crop:Wd.neutral_grav *));
            "shrinkable" |> pString |>$ Ui.resize ~sw:1;
          ];
        W.hbox
          [
            (let og =
               W.string "123456789000000000000000000000000000000000000000000000000000end"
             in
             og
             |> Lwd.pure
             |> W.scroll_area
             |> Wd.border_box_focusable
                  ~focus:focus2
                  ~pad_h:4
                  ~scaling:(`Shrinkable (15, 1))
             (* |>$ Ui.resize ~pad:Wd.neutral_grav ~crop:Wd.neutral_grav *));
            "shrinkable" |> pString |>$ Ui.resize ~sw:1;
          ];
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
(* |>$ Ui.keyboard_area (function *)
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
  W.hbox
    [
      Wd.v_window_stack2
        [
          is_focused_widget ();
          W.string "hi this is the first level"
          |> Lwd.pure
          |> W.scroll_area
          |> Wd.border_box_focusable;
          Wd.v_window_stack2
            [
              W.string "hi this is the first level"
              |> Lwd.pure
              |> W.scroll_area
              |> Wd.border_box_focusable;
              W.string "hi this is the second level"
              |> Lwd.pure
              |> W.scroll_area
              |> Wd.border_box_focusable ~focus:start;
              Wd.v_window_stack2
                [
                  W.string "hi this is the first level"
                  |> Lwd.pure
                  |> W.scroll_area
                  |> Wd.border_box_focusable;
                  W.string "hi this is the second level"
                  |> Lwd.pure
                  |> W.scroll_area
                  |> Wd.border_box_focusable;
                ];
            ];
        ];
      Wd.v_window_stack2
        [
          W.string "hi this is the first level"
          |> Lwd.pure
          |> W.scroll_area
          |> Wd.border_box_focusable;
          W.string "hi this is the second level"
          |> Lwd.pure
          |> W.scroll_area
          |> Wd.border_box_focusable;
        ];
    ]
  |> navMode
;;

let w_5 =
  let focus = Focus.make () in
  Focus.request focus;
  Wd.v_window_stack
    ~focus
    [
      is_focused_widget2;
      is_focused_widget2;
      Wd.h_window_stack
        [
          is_focused_widget2;
          is_focused_widget2;
          Wd.v_window_stack
            [
              is_focused_widget2;
              is_focused_widget2;
              Wd.h_window_stack
                [
                  is_focused_widget2;
                  is_focused_widget2;
                  Wd.v_window_stack [ is_focused_widget2; is_focused_widget2 ];
                ];
            ];
        ];
      Wd.h_window_stack [ is_focused_widget2 ];
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
           W.string "hi this is the first level" |> Lwd.pure;
           (let$ _ = Lwd.get reaction in
            result $= Lwd.peek result ^ "->inner";
            W.string " hi this is the second level");
         ]);
      (let$ result = Lwd.get result in
       W.string result);
    ]
  |>$ Ui.keyboard_area (function
    | `Enter, _ ->
      reaction $= false;
      `Handled
    | _ ->
      `Unhandled)
;;

let w_6 =
  let state = Lwd.var W.default_scroll_state in
  W.vbox
    [
      Ui.vcat [ W.string "hi";W.string "50"; W.string "there"; W.string "mate" ]
      |> Lwd.pure
      |> W.vscroll_area ~change:(fun _ x -> state $= x) ~state:(Lwd.get state)
      |>$ Ui.resize ~h:2 ~sh:0;
      Wd.h_rule |> Lwd.pure;
      W.file_select ~on_select:(fun x -> ()) ();
      W.unfoldable
        (W.string "click to unfold" |> Lwd.pure)
        (fun () -> W.string "I'm unfolded" |> Lwd.pure);
    ]
;;
let w_7 =
  let state = Lwd.var (W.default_scroll_state,W.default_scroll_state) in
  W.vbox
    [
    Wd.h_window_stack2[
      Ui.vcat [ W.string "hi";W.string "50"; W.string "there345678987654356789876545end"; W.string "mate" ]
      |> Lwd.pure
      |> Wd.scroll_area 
      |>$ Ui.resize ~mw:(15)
      ;
      W.string "|"|>Lwd.pure
      ];
    Wd.h_window_stack2[
      Ui.vcat [ W.string "hi";W.string "50"; W.string "there345678987654356789876545end"; W.string "mate" ]
      |> Lwd.pure
      |> Wd.v_scroll_area 
      |>$ Ui.resize  ~h:2 ~sh:(0)
      ;
      W.string "|"|>Lwd.pure
      ];
      W.string "----"|>Lwd.pure
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
   | 7->
     w_7
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
    | `Key (`ASCII 'q', [ `Ctrl ]) ->
      quit $= true;
      `Handled
    | _ ->
      `Unhandled)
;;

(* w_3|>Lwd.observe|>Lwd.quick_sample|> *)
(* Ui.pp Format.std_formatter ;; *)
Nottui.Ui_loop.run ~renderer ~quit main_ui
