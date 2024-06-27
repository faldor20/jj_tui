(**Widgets that are designed to overlay some exisiting Ui*)

open Nottui_main
open Shared
open Lwd_infix

open struct
  module BB = Border_box
  module W = Nottui_widgets
end

open Notty

let size_logger ui =
  let size = Lwd.var (-1, -1) in
  W.vbox
    [ (size |> Lwd.get |>$ fun (w, h) -> W.fmt "w:%d,h:%d" w h)
    ; ui
      |>$ Ui.size_sensor (fun ~w ~h ->
        if Lwd.peek size <> (w, h) then Lwd.set size (w, h))
    ]
;;

let set_bg ~attr ui =
  let size = Lwd.var (0, 0) in
  W.zbox
    [ (size
       |> Lwd.get
       |>$ fun (w, h) -> I.char attr ' ' w h |> Ui.atom |> Ui.resize ~w:0 ~h:0)
    ; ui |>$ Ui.size_sensor (fun ~w ~h -> if (w, h) <> Lwd.peek size then size $= (w, h))
    ]
;;

let set_bg_static ~attr ui =
  let w, h = Ui.layout_width ui, Ui.layout_height ui in
  Ui.zcat [ I.char attr ' ' w h |> Ui.atom |> Ui.resize ~w:0 ~h:0; ui ]
;;

let clear_bg ui = set_bg ~attr:A.empty ui

(** Internal only function for building prompts. Just deals with the most basic part of the prompt so it can be extended with some custom content.
    The caller of the function is expected to call
    [Focus.release_reversable focus]
    and set [show_prompt] to [None]
    on_exit *)
let prompt_internal ?pad_w ?pad_h ~focus ~show_prompt ui =
  (*Build the ui so that it is either the prompt or nothing depending on whether show prompt is enabled*)
  let prompt_ui =
    let$* show_prompt_val = show_prompt in
    let prompt_ui =
      show_prompt_val
      |> Option.map
         @@ fun (label, label_bottom, on_exit, prompt_content) ->
         (*we need focus because the base ui is rendering first and so *)
         Focus.request_reversable focus;
         let$* label_bottom = label_bottom in
         (*prefill the prompt if we want to *)
         prompt_content
         |> BB.focusable ?pad_w ?pad_h ~focus ~label_top:label ?label_bottom
         |> clear_bg
         |> Lwd.map2 (Focus.status focus) ~f:(fun focus_status ui ->
           ui
           |> Ui.event_filter ~focus:focus_status (fun event ->
             match event with
             | `Key (`Escape, _) ->
               on_exit `Closed;
               `Handled
             | _ -> `Unhandled))
    in
    prompt_ui |> Option.value ~default:(Ui.empty |> Lwd.pure)
  in
  (*Now that we have the prompt ui we layer it ontop of the normal ui using zbox.
    My hope is that by not directly nesting them this will allow the ui to not re-render when the prompt appears*)
  W.zbox [ ui; prompt_ui |> Lwd.map ~f:(Ui.resize ~pad:neutral_grav) ]
;;

type text_prompt_data =
  { label : string
  ; pre_fill : string
  ; on_exit : [ `Closed | `Finished of string ] -> unit
  }

let text_prompt
  ?pad_h
  ?pad_w
  ?(modify_body = fun x -> x)
  ?(focus = Focus.make ())
  ?(char_count = false)
  ~(show_prompt_var : text_prompt_data Option.t Lwd.var)
  ui
  =
  let prompt_input = Lwd.var ("", 0) in
  let prompt_val = Lwd.get prompt_input in
  (*Build the ui so that it is either the prompt or nothing depending on whether show prompt is enabled*)
  let prompt_args =
    let$ show_prompt_val = Lwd.get show_prompt_var in
    show_prompt_val
    |> Option.map
       @@ fun { label; pre_fill; on_exit } ->
       let on_exit result =
         Focus.release_reversable focus;
         show_prompt_var $= None;
         prompt_input $= ("", 0);
         on_exit result
       in
       (*we need focus because the base ui is rendering first and so *)
       Focus.request_reversable focus;
       (*prefill the prompt if we want to *)
       if prompt_input |> Lwd.peek |> fst == ""
       then prompt_input $= (pre_fill, pre_fill |> String.length);
       let prompt_field =
         W.zbox
           [ W.string ~attr:A.(st underline) "                                       "
             |> Lwd.pure
           ; W.edit_field
               prompt_val
               ~on_change:(fun state -> Lwd.set prompt_input state)
               ~on_submit:(fun (str, _) -> on_exit (`Finished str))
           ]
         |> modify_body
       in
       let label_bottom =
         let$ prompt_val, _ = prompt_val in
         if char_count then Some (prompt_val |> String.length |> Int.to_string) else None
       in
       label, label_bottom, on_exit, prompt_field
  in
  prompt_internal ?pad_w ?pad_h ~focus ~show_prompt:prompt_args ui
;;

type 'a selection_list_prompt_data =
  { label : string
  ; items : 'a Selection_list.selectable_item list Lwd.t
  ; on_exit : [ `Closed | `Finished of 'a ] -> unit
  }

(* TODO: Write a ui resize function that takes an optional version of the layout spec where eveyr field is optional too, then we can use that when we want to make a widget with a custom internal state, or i can just use w mw h mw etc *)

let selection_list_prompt
  ?pad_w
  ?pad_h
  ?(modify_body = fun x -> x)
  ?(focus = Focus.make ())
  ~show_prompt_var
  ui
  =
  (*Build the ui so that it is either the prompt or nothing depending on whether show prompt is enabled*)
  let prompt_args =
    let$ show_prompt_val = Lwd.get show_prompt_var in
    show_prompt_val
    |> Option.map
       @@ fun { label; items; on_exit } ->
       let on_exit result =
         Focus.release_reversable focus;
         show_prompt_var $= None;
         on_exit result
       in
       (*we need focus because the base ui is rendering first and so *)
       Focus.request_reversable focus;
       (*prefill the prompt if we want to *)
       let prompt_field =
         Selection_list.selection_list_custom
           ~focus
           ~custom_handler:(fun item key ->
             match key with
             | `Enter, [] ->
               `Finished item.data |> on_exit;
               `Handled
             | _ -> `Unhandled)
           items
         |> modify_body
       in
       let label_bottom =
         let$ items = items in
         Some (items |> List.length |> Printf.sprintf "%d items")
       in
       label, label_bottom, on_exit, prompt_field
  in
  prompt_internal ?pad_w ?pad_h ~focus ~show_prompt:prompt_args ui
;;

let popup ~show_popup_var ui =
  let popup_ui =
    let$* show_popup = Lwd.get show_popup_var in
    match show_popup with
    | Some (content, label) ->
      let prompt_field = content in
      prompt_field |>$ Ui.resize ~w:5 |> BB.box ~label_top:label |> clear_bg
    | None -> Ui.empty |> Lwd.pure
  in
  W.zbox [ ui; popup_ui |>$ Ui.resize ~crop:neutral_grav ~pad:neutral_grav ]
;;
