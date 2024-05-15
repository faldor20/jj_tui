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

let dynamic_width_limit ?wMax ?(w = 0) ~sw ?h ?sh f =
  let width = Lwd.var w in
  let body = f (Lwd.get width |> Lwd.map ~f:(fun x -> x - 5)) in
  body
  |> Lwd.map ~f:(fun ui ->
    ui
    |> Wd.border_box
    |> Ui.resize ~w ~sw ?h ?sh ?mw:wMax
    |> Wd.border_box
    |> Ui.size_sensor (fun ~w ~h:_ -> if Lwd.peek width <> w then Lwd.set width w))
;;

let w_0 =
  W.vbox
    [
      Ui.vcat
        [
          W.string "border around a simple text box";
          Wd.border_box (W.string "hii");
          Wd.border_box (W.string "hii" |> Ui.resize ~w:20);
          Wd.border_box (W.string "hii" |> Ui.resize ~w:2);
          Wd.border_box (W.string "hii" |> Ui.resize ~w:2);
          W.string
            "note how given the chance an element will stretch to fill space, even if \
             it's content doesn't need to ";
          Ui.hcat
            [
              Wd.border_box (W.string "hii") |> Ui.resize ~w:2 ~sw:1;
              W.string "other end" |> Ui.resize ~sw:1;
            ];
          W.string "even if its parent actually sets a width  ";
          Wd.border_box
            (Ui.vcat
               [
                 W.string "hii" |> Ui.resize ~w:2 ~sw:1; W.string "hii" |> Ui.resize ~w:2;
               ]
             |> Ui.resize ~w:10);
        ]
      |> Lwd.pure;
      W.string
        "Using dynamic_width we can give our component a dynamic width that can be \
         easily read by parents. "
      |> Lwd.pure;
      W.string
        "This is becasue it uses a sizeSensor to get the real rendered size and then set \
         the width to that "
      |> Lwd.pure;
      W.hbox
        [
          Wd.dynamic_width ~sw:1 (fun w ->
            let$ w = w in
            W.string "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
            |> Ui.resize ~w
            |> Wd.border_box);
          W.string "other hiiii" |> Ui.resize ~sw:1 ~mw:10 |> Lwd.pure;
        ];
      W.string "but the width isn't retained for external elements" |> Lwd.pure;
      W.hbox
        [
          Wd.dynamic_width ~sw:1 (fun w ->
            let$ w = w in
            W.string "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" |> Ui.resize ~w)
          |>$ Wd.border_box;
          W.string "other hiiii" |> Ui.resize ~sw:1 |> Lwd.pure;
        ];
      W.hbox
        [
          Wd.dynamic_width ~sw:1 (fun w ->
            let$ w = w in
            let ui = W.string "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" in
            ui |> Ui.resize ~w:(min w (Ui.layout_width ui)) |> Wd.border_box);
          W.string "other hiiii" |> Ui.resize ~sw:1 |> Lwd.pure;
        ];
      "we can set a max width though!" |> pString;
      W.hbox
        [
          Wd.dynamic_width ~sw:1 ~mw:3 (fun w ->
            let$ w = w in
            W.string "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" |> Ui.resize ~w ~mw:3)
          |>$ Wd.border_box;
          W.string "other hiiii" |> Ui.resize ~sw:1 |> Lwd.pure;
        ];
      W.hbox
        [
          Wd.dynamic_width ~sw:1 (fun w ->
            let$ w = w in
            let ui = W.string "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" in
            ui
            |> Ui.resize
                 ~w:(min w (Ui.layout_width ui))
                 ~pad:Wd.neutral_grav
                 ~crop:Wd.neutral_grav
                 ~mw:5
            |> Wd.border_box);
          W.string "other hiiii" |> Ui.resize ~sw:1 |> Lwd.pure;
        ];
    ]
  |> W.scroll_area
;;

let dw_box ~sw ui =
  let ui_w = Ui.layout_width ui in
  let wholeWidth = Lwd.var 0 in
  ( dynamic_width_limit ~sw ~wMax:(Ui.layout_width ui) @@ fun w ->
    let$* w = w in
    ui
    |> Lwd.pure
    |> W.scroll_area
    |>$ Ui.resize ~sw:1 ~mw:ui_w
    |> Wd.ui_outline2
    |>$ Ui.resize ~sw:0 ~w )
  |>$ Ui.resize ~sw:1 ~mw:(ui_w + 7)
;;

let w_1 =
  W.vbox
    [
      pString
        "This is the soltion, because the streatching propegates upwards we have to stop \
         the stretch at some point";
      W.hbox
        [
          pString "hiiiiiiiiiii"
          |> W.scroll_area
          |>$ Ui.resize ~w:2 ~sw:1
          |>$ Wd.border_box
          |>$ Ui.resize ~sw:0;
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let w_var = Lwd.var 0 in
           let$* width = Lwd.get w_var in
           let ui =
             pString
               "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiend"
             |> W.scrollbox
             |>$ Ui.resize ~pad:Wd.neutral_grav
             |>$ Ui.resize ~sw:0 ~w:30
             |>$ Ui.size_sensor (fun ~w ~h -> if w != width then w_var $= w)
             |>$ Wd.border_box
           in
           W.zbox
             [
               W.vbox [ ui; I.strf "w:%d" width |> Ui.atom |> Lwd.pure ]
               (* Wd.outline A.empty w 2 |>Ui.atom|>Lwd.pure *);
             ]);
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let input =
             W.string
               "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
           in
           input
           |> Lwd.pure
           |> W.scroll_area
           |>$ Ui.resize ~w:0 ~sw:1 ~mw:(input |> Ui.layout_width)
           |>$ Wd.border_box
           |>$ Ui.resize ~sw:1 ~mw:(input |> Ui.layout_width));
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let input =
             W.string
               "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
           in
           input |> dw_box ~sw:1);
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let input =
             W.string
               "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiend"
           in
           input |> Lwd.pure |> W.scroll_area |>$ Ui.resize ~sw:0 |> Wd.ui_outline2);
          "other end" |> pString;
        ];
      W.hbox
        [
          (let input =
             W.string
               "3hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
           in
           input |> Lwd.pure |> W.scroll_area |> Wd.ui_outline3);
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          dynamic_width_limit ~sw:1 (fun x ->
            let$* width = x in
            let input =
              W.string
                "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
            in
            input |> Lwd.pure |> W.scroll_area |>$ Ui.resize ~w:(width - 3));
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
    ]
;;

let w_2 =
  W.vbox
    [
      W.hbox
        [
          Wd.dynamic_width ~sw:1 (fun x ->
            let$* width = x in
            let input =
              W.string
                "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
            in
            input |> Lwd.pure |>$ Ui.resize ~w:width)
          |>$ Ui.resize ~sw:1;
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let og =
             W.string
               "h123456789012345678901234567890123456789123456789020934871028374081723049871023984701982734end"
           in
           og
           |> Lwd.pure
           |> W.scroll_area
           |>$ Ui.resize ~w:0 ~sw:1
           |> Wd.border_box2 ~mw:(og |> Ui.layout_width));
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let og = W.string "1234567890" in
           og
           |> Lwd.pure
           |> W.scroll_area
           |>$ Ui.resize ~sw:1 ~w:0 
           |> Wd.border_box2 
           (* |>$ Ui.resize ~pad:Wd.neutral_grav ~crop:Wd.neutral_grav *) 
            );
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let og = W.string "1234567890000000000000000000000000234567890000000000000000000000000end" in
           og
           |> Lwd.pure
           |> W.scroll_area
           |>$ Ui.resize ~sw:1 ~w:10 
           |> Wd.border_box2 
           |>$ Ui.resize ~mw:(og|>Ui.layout_width|>(+)6) 
            );
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let og = W.string "123456789000000000000000000000000000000000000000000000000000end" in
           og
           |> Lwd.pure
           |> W.scroll_area
           |> Wd.border_box3 ~scaling:(`Shrinkable (15,1)) 
           (* |>$ Ui.resize ~pad:Wd.neutral_grav ~crop:Wd.neutral_grav *) 
            );
          "shrinkable" |> pString |>$ Ui.resize ~sw:1;
        ];
      W.hbox
        [
          (let og = W.string "1234567890" in
           og|>Lwd.pure|>Wd.border_box_scrollable);
          "other end" |> pString |>$ Ui.resize ~sw:1;
        ]
      |> Wd.border_box2;
    ]
;;

let w_3 =
  W.vbox
    [
      W.flex_box
        ~w:(30 |> Lwd.pure)
        [ "number 1 r:1" |> pString; "number2 r:1" |> pString; "number3 r:1" |> pString ]
      |> Wd.border_box2;
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
    | `Key (`ASCII 'q', _) ->
      quit $= true;
      `Handled
    | _ ->
      `Unhandled)
;;

Nottui.Ui_loop.run ~quit main_ui
