open Notty
open Nottui_main
open Lwd_infix
open Shared

let dynamic_size ?(w = 10) ~sw ?(h = 10) ~sh f =
  let size = Lwd.var (w, h) in
  let body = f (Lwd.get size) in
  body
  |> Lwd.map ~f:(fun ui ->
    ui
    |> Ui.resize ~w ~sw ~h ~sh
    |> Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then Lwd.set size (w, h)))
;;

(*
   design for a windowing system:
   fundinmental primative:
   window stack:
   when inside a window stack you can navigate up and down your stack.
   if you would also like to navigate to stacks sideways you just wrap then in a horizontal window stack.
*)
let v_window_stack ~focus windows =
  let focused = ref 0 in
  let is_focused = ref false in
  let windows, focuses =
    windows
    |> List.map (fun window_maker ->
      let focus = Focus.make () in
      window_maker ~focus, focus)
    |> List.split
  in
  focuses |> List.hd |> Focus.request;
  (let$ ui = vbox windows
   and$ focus = focus |> Focus.status in
   if (not !is_focused) && Focus.has_focus focus
   then (
     is_focused := true;
     List.nth focuses !focused |> Focus.request)
   else if !is_focused && not (Focus.has_focus focus)
   then is_focused := false;
   ui
   |> Ui.join_x (if !is_focused then string "f" else string "u")
   |> Ui.keyboard_area
        ~focus
        (if !is_focused
         then
           function
           | `Arrow `Down, [ `Ctrl ] ->
             let focused_idx = !focused in
             (* set the focus to be the next item if possible *)
             List.nth_opt focuses (focused_idx + 1)
             |> Option.iter (fun x ->
               List.nth focuses focused_idx |> Focus.release;
               Focus.request x;
               focused := focused_idx + 1);
             `Handled
           | `Arrow `Up, [ `Ctrl ] ->
             let focused_idx = !focused in
             let target_idx = focused_idx - 1 in
             if target_idx >= 0
             then
               (* set the focus to be the previous item if possible *)
               List.nth_opt focuses target_idx
               |> Option.iter (fun x ->
                 List.nth focuses focused_idx |> Focus.release;
                 Focus.request x;
                 focused := focused_idx - 1);
             `Handled
           | _ ->
             `Unhandled
         else fun _ -> `Unhandled))
  |> Border_box.with_border_attr
       (let$ focus = focus |> Focus.status |>$ Focus.has_focus in
        if focus then A.fg A.blue else A.empty)
;;

let h_window_stack ~focus windows =
  let focused = ref 0 in
  let is_focused = ref false in
  let windows, focuses =
    windows
    |> List.map (fun window_maker ->
      let focus = Focus.make () in
      window_maker ~focus, focus)
    |> List.split
  in
  (let$ ui = hbox windows
   and$ focus = focus |> Focus.status in
   if (not !is_focused) && Focus.has_focus focus
   then (
     is_focused := true;
     List.nth focuses !focused |> Focus.request)
   else if !is_focused && not (Focus.has_focus focus)
   then is_focused := false;
   ui
   |> Ui.keyboard_area
        ~focus
        (if !is_focused
         then
           function
           | `Arrow `Right, [ `Ctrl ] ->
             let focused_idx = !focused in
             (* set the focus to be the next item if possible *)
             List.nth_opt focuses (focused_idx + 1)
             |> Option.iter (fun x ->
               List.nth focuses focused_idx |> Focus.release;
               Focus.request x;
               focused := focused_idx + 1);
             `Handled
           | `Arrow `Left, [ `Ctrl ] ->
             let focused_idx = !focused in
             let target_idx = focused_idx - 1 in
             if target_idx >= 0
             then
               (* set the focus to be the previous item if possible *)
               List.nth_opt focuses target_idx
               |> Option.iter (fun x ->
                 List.nth focuses focused_idx |> Focus.release;
                 Focus.request x;
                 focused := focused_idx - 1);
             `Handled
           | _ ->
             `Unhandled
         else fun _ -> `Unhandled))
  |> Border_box.with_border_attr
       (let$ focus = focus |> Focus.status |>$ Focus.has_focus in
        if focus then A.fg A.blue else A.empty)
;;
