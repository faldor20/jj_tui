open Notty
open Nottui_main
open Nottui_widgets
open Lists

(**
All these widgets I keep around becasue they were in nottui, but I am not personally using them and can't speak to their quality or stablity
*)

let attr_menu_main = A.(bg green ++ fg black)
let attr_menu_sub = A.(bg lightgreen ++ fg black)

type window_manager =
  { overlays : ui Lwd.t Lwd_table.t
  ; view : ui Lwd.t
  }

let window_manager base =
  let overlays = Lwd_table.make () in
  let composition =
    Lwd.join (Lwd_table.reduce (Lwd_utils.lift_monoid Ui.pack_z) overlays)
  in
  let view =
    Lwd.map2 base composition ~f:(fun base composite ->
      Ui.join_z base (Ui.resize_to (Ui.layout_spec base) composite))
  in
  { overlays; view }
;;

let window_manager_view wm = wm.view
let window_manager_overlays wm = wm.overlays

let menu_overlay wm g ?(dx = 0) ?(dy = 0) body around =
  let sensor ~x ~y ~w ~h () =
    let row = Lwd_table.append (window_manager_overlays wm) in
    let h_pad =
      match Gravity.h g with
      | `Negative -> Ui.space (x + dx) 0
      | `Neutral -> Ui.space (x + dx + (w / 2)) 0
      | `Positive -> Ui.space (x + dx + w) 0
    in
    let v_pad =
      match Gravity.v g with
      | `Negative -> Ui.space 0 (y + dy)
      | `Neutral -> Ui.space 0 (y + dy + (h / 2))
      | `Positive -> Ui.space 0 (y + dy + h)
    in
    let view =
      Lwd.map body ~f:(fun body ->
        let body =
          let pad = Ui.space 1 0 in
          Ui.join_x pad (Ui.join_x body pad)
        in
        let bg = Ui.resize_to (Ui.layout_spec body) ~bg:A.(bg lightgreen) Ui.empty in
        let catchall =
          Ui.mouse_area
            (fun ~x:_ ~y:_ -> function
              | `Left ->
                Lwd_table.remove row;
                `Handled
              | _ -> `Handled)
            (Ui.resize ~sw:1 ~sh:1 ~mw:1000 ~mh:1000 Ui.empty)
        in
        Ui.join_z catchall @@ Ui.join_y v_pad @@ Ui.join_x h_pad @@ Ui.join_z bg body)
    in
    Lwd_table.set row view
  in
  Ui.transient_sensor sensor around
;;

let main_menu_item wm text f =
  let text = string ~attr:attr_menu_main (" " ^ text ^ " ") in
  let refresh = Lwd.var () in
  let overlay = ref false in
  let on_click ~x:_ ~y:_ = function
    | `Left ->
      overlay := true;
      Lwd.set refresh ();
      `Handled
    | _ -> `Unhandled
  in
  Lwd.map (Lwd.get refresh) ~f:(fun () ->
    let ui = Ui.mouse_area on_click text in
    if !overlay
    then (
      overlay := false;
      menu_overlay wm (Gravity.make ~h:`Negative ~v:`Positive) (f ()) ui)
    else ui)
;;

let sub_menu_item wm text f =
  let text = string ~attr:attr_menu_sub text in
  let refresh = Lwd.var () in
  let overlay = ref false in
  let on_click ~x:_ ~y:_ = function
    | `Left ->
      overlay := true;
      Lwd.set refresh ();
      `Handled
    | _ -> `Unhandled
  in
  Lwd.map (Lwd.get refresh) ~f:(fun () ->
    let ui = Ui.mouse_area on_click text in
    if !overlay
    then (
      overlay := false;
      menu_overlay wm (Gravity.make ~h:`Positive ~v:`Negative) (f ()) ui)
    else ui)
;;

let sub_entry text f =
  let text = string ~attr:attr_menu_sub text in
  let on_click ~x:_ ~y:_ = function
    | `Left ->
      f ();
      `Handled
    | _ -> `Unhandled
  in
  Ui.mouse_area on_click text
;;

(*------- scrolling------*)

type scrollbox_state =
  { w : int
  ; h : int
  ; x : int
  ; y : int
  }

let adjust_offset visible total off =
  let off = if off + visible > total then total - visible else off in
  let off = if off < 0 then 0 else off in
  off
;;

let decr_if x cond = if cond then x - 1 else x
let scrollbar_bg = Notty.A.gray 4
let scrollbar_fg = Notty.A.gray 7
let scrollbar_click_step = 3 (* Clicking scrolls one third of the screen *)
let scrollbar_wheel_step = 8 (* Wheel event scrolls 1/8th of the screen *)

let hscrollbar visible total offset ~set =
  let prefix = offset * visible / total in
  let suffix = (total - offset - visible) * visible / total in
  let handle = visible - prefix - suffix in
  let render size color = Ui.atom Notty.(I.char (A.bg color) ' ' size 1) in
  let mouse_handler ~x ~y:_ = function
    | `Left ->
      if x < prefix
      then (
        set (offset - maxi 1 (visible / scrollbar_click_step));
        `Handled)
      else if x > prefix + handle
      then (
        set (offset + maxi 1 (visible / scrollbar_click_step));
        `Handled)
      else
        `Grab
          ( (fun ~x:x' ~y:_ -> set (offset + ((x' - x) * total / visible)))
          , fun ~x:_ ~y:_ -> () )
    | `Scroll dir ->
      let dir =
        match dir with
        | `Down -> 1
        | `Up -> -1
      in
      set (offset + (dir * maxi 1 (visible / scrollbar_wheel_step)));
      `Handled
    | _ -> `Unhandled
  in
  let ( ++ ) = Ui.join_x in
  Ui.mouse_area
    mouse_handler
    (render prefix scrollbar_bg
     ++ render handle scrollbar_fg
     ++ render suffix scrollbar_bg)
;;

let vscrollbar visible total offset ~set =
  let prefix = offset * visible / total in
  let suffix = (total - offset - visible) * visible / total in
  let handle = visible - prefix - suffix in
  let render size color = Ui.atom Notty.(I.char (A.bg color) ' ' 1 size) in
  let mouse_handler ~x:_ ~y = function
    | `Left ->
      if y < prefix
      then (
        set (offset - maxi 1 (visible / scrollbar_click_step));
        `Handled)
      else if y > prefix + handle
      then (
        set (offset + maxi 1 (visible / scrollbar_click_step));
        `Handled)
      else
        `Grab
          ( (fun ~x:_ ~y:y' -> set (offset + ((y' - y) * total / visible)))
          , fun ~x:_ ~y:_ -> () )
    | `Scroll dir ->
      let dir =
        match dir with
        | `Down -> 1
        | `Up -> -1
      in
      set (offset + (dir * maxi 1 (visible / scrollbar_wheel_step)));
      `Handled
    | _ -> `Unhandled
  in
  let ( ++ ) = Ui.join_y in
  Ui.mouse_area
    mouse_handler
    (render prefix scrollbar_bg
     ++ render handle scrollbar_fg
     ++ render suffix scrollbar_bg)
;;

let scrollbox t =
  (* Keep track of scroll state *)
  let state_var = Lwd.var { w = 0; h = 0; x = 0; y = 0 } in
  (* Keep track of size available for display *)
  let update_size ~w ~h =
    let state = Lwd.peek state_var in
    if state.w <> w || state.h <> h then Lwd.set state_var { state with w; h }
  in
  let measure_size body =
    Ui.size_sensor update_size (Ui.resize ~w:0 ~h:0 ~sw:1 ~sh:1 body)
  in
  (* Given body and state, composite scroll bars *)
  let compose_bars body state =
    let bw, bh = Ui.layout_width body, Ui.layout_height body in
    (* Logic to determine which scroll bar should be visible *)
    let hvisible = state.w < bw
    and vvisible = state.h < bh in
    let hvisible = hvisible || (vvisible && state.w = bw) in
    let vvisible = vvisible || (hvisible && state.h = bh) in
    (* Compute size and offsets based on visibility *)
    let state_w = decr_if state.w vvisible in
    let state_h = decr_if state.h hvisible in
    let state_x = adjust_offset state_w bw state.x in
    let state_y = adjust_offset state_h bh state.y in
    (* Composite visible scroll bars *)
    let crop b = Ui.resize ~sw:1 ~sh:1 ~w:0 ~h:0 (Ui.shift_area state_x state_y b) in
    let set_vscroll y =
      let state = Lwd.peek state_var in
      if state.y <> y then Lwd.set state_var { state with y }
    in
    let set_hscroll x =
      let state = Lwd.peek state_var in
      if state.x <> x then Lwd.set state_var { state with x }
    in
    let ( <-> ) = Ui.join_y
    and ( <|> ) = Ui.join_x in
    match hvisible, vvisible with
    | false, false -> body
    | false, true -> crop body <|> vscrollbar state_h bh state_y ~set:set_vscroll
    | true, false -> crop body <-> hscrollbar state_w bw state_x ~set:set_hscroll
    | true, true ->
      crop body
      <|> vscrollbar state_h bh state_y ~set:set_vscroll
      <-> (hscrollbar state_w bw state_x ~set:set_hscroll <|> Ui.space 1 1)
  in
  (* Render final box *)
  Lwd.map2 t (Lwd.get state_var) ~f:(fun ui size -> measure_size (compose_bars ui size))
;;

(** A mouse_based file selection widget that opens at the current path *)
let file_select ?(abs = false) ?filter ~(on_select : string -> unit) () : Ui.t Lwd.t =
  let rec aux ~fold path =
    try
      let p_rel = if path = "" then "." else path in
      if Sys.is_directory p_rel
      then (
        let ui () =
          let arr = Sys.readdir p_rel in
          let l = Array.to_list arr |> List.map (Filename.concat path) in
          (* apply potential filter *)
          let l =
            match filter with
            | None -> l
            | Some f -> List.filter f l
          in
          let l = Lwd.return @@ List.sort String.compare l in
          vlist_with ~bullet:"" (aux ~fold:true) l
        in
        if fold
        then unfoldable ~folded_by_default:true (Lwd.return @@ string @@ path ^ "/") ui
        else ui ())
      else Lwd.return @@ button ~attr:A.(st underline) path (fun () -> on_select path)
    with
    | e ->
      Lwd.return
      @@ Ui.vcat
           [ printf ~attr:A.(bg red) "cannot list directory %s" path
           ; string @@ Printexc.to_string e
           ]
  in
  let start = if abs then Sys.getcwd () else "" in
  aux ~fold:false start
;;

(** Tab view, where exactly one element of [l] is shown at a time. *)
let tabs (tabs : (string * (unit -> Ui.t Lwd.t)) list) : Ui.t Lwd.t =
  let open Lwd.Infix in
  match tabs with
  | [] -> Lwd.return Ui.empty
  | _ ->
    let cur = Lwd.var 0 in
    Lwd.get cur
    >>= fun idx_sel ->
    let _, f = List.nth tabs idx_sel in
    let tab_bar =
      tabs
      |> List.mapi (fun i (s, _) ->
        let attr = if i = idx_sel then A.(st underline) else A.empty in
        let tab_annot = printf ~attr "[%s]" s in
        Ui.mouse_area
          (fun ~x:_ ~y:_ l ->
            if l = `Left
            then (
              Lwd.set cur i;
              `Handled)
            else `Unhandled)
          tab_annot)
      |> Ui.hcat
    in
    f () >|= Ui.join_y tab_bar
;;

