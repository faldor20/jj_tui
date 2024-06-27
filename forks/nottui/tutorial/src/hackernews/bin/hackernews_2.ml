(*$#S13*)
open Nottui
open Notty
open Hackernews_api
open Lwd_infix

(*$#S5*)
let post_ui ({ title; url; score; comments; _ } : Hackernews_api.post) =
  let website = List.nth (String.split_on_char '/' url) 2 in
  Ui.vcat
    [ Ui.hcat
        [ W.string ~attr:A.(st bold) title
        ; W.string " "
        ; W.printf ~attr:A.(st italic ++ fg lightblack) "(%s)" website
        ]
    ; Ui.hcat
        [ W.printf ~attr:A.(st italic) "%d points" score
        ; W.string "  "
        ; W.printf ~attr:A.(st italic) "%d comments" comments
        ]
    ]
  |> Ui.resize ~sw:1 ~mw:10000
  |> Lwd.pure
  |> W.Box.focusable
;;

(*$#E5*)

(*$#S6*)
let show_prompt_var = Lwd.var None
let sorting_mode_var = Lwd.var `Points
(*$#E6*)

(*$#S7*)
let sorting_prompt ui =
  let open W.Overlay in
  let open W.Lists in
  let res =
    ui
    (*$#S8*)
    |> W.Overlay.selection_list_prompt
         ~modify_body:(Lwd.map ~f:(Ui.resize ~sw:1 ~mw:20))
         ~show_prompt_var
    (*$#E8*)
    (*$#S9*)
    |>$ Ui.keyboard_area (function
      | `ASCII 's', [] ->
        (*funcion to handle when the prompt is closed using escape or enter *)
        let on_exit x =
          match x with
          | `Closed -> ()
          | `Finished sorting -> sorting_mode_var $= sorting
        in
        let prompt =
          { label = "Sorting method"
          ; items =
              Lwd.pure
                [ { data = `Points; ui = W.Lists.selectable_item (W.string "Points") }
                ; { data = `Comments; ui = W.Lists.selectable_item (W.string "Comments") }
                ]
          ; on_exit
          }
        in
        show_prompt_var $= Some prompt;
        `Handled
      | _ -> `Unhandled)
    (*$#E9*)
  in
  res
;;

(*$#E7*)

(*$#S10*)
let get_sort_func sorting =
  match sorting with
  | `Points -> fun a b -> Int.compare b.score a.score
  | `Comments -> fun a b -> Int.compare b.comments a.comments
;;

(*$#E10*)

(*$#S11*)
let shortcuts = Ui.vcat [ Ui.hcat [ W.string "[S]orting" ] ]

let main_ui =
(*$#S15*)
  let sorted_by_ui =
    let$ sorting = Lwd.get sorting_mode_var in
    (match sorting with
     | `Points -> "Points"
     | `Comments -> "Comments")
    |> W.fmt "Sorted by %s"
  in
(*$#E15*)
(*$#S14*)
  let posts =
    let$* sort_mode = Lwd.get sorting_mode_var in
    let sort_func = get_sort_func sort_mode in
    Hackernews_api.fake_posts ()
    |> List.sort sort_func
    |> List.map post_ui
    |> W.vbox
    |> W.Scroll.v_area
  in
(*$#E14*)
(*$#S12*)
  W.vbox
    [ 
    sorted_by_ui|>W.Box.box ~pad_w:1 ~pad_h:0;
    posts |> W.Box.box ~pad_w:1 ~pad_h:0
    ; shortcuts |> Ui.resize ~sw:1 ~mw:10000 |> Lwd.pure |> W.Box.box ~pad_w:1 ~pad_h:0
    ]
  |> sorting_prompt
;;

(*$#E12*)

(*$#E11*)

let () = Nottui.Ui_loop.run ~quit_on_escape:false main_ui

(*$#E13*)
