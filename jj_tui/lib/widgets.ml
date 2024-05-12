open Notty
open Nottui
open Lwd_infix
open! Util
module W = Nottui_widgets

let dynamic_width ?(w = 0) ?mw ~sw ?h ?sh f =
  let width = Lwd.var w in
  let body = f (Lwd.get width) in
  body
  |> Lwd.map ~f:(fun ui ->
    ui
    |> Ui.resize ~w ~sw ?mw ?h ?sh
    |> Ui.size_sensor (fun ~w ~h:_ -> if Lwd.peek width <> w then Lwd.set width w))
;;

let dynamic_size ?(w = 10) ~sw ?(h = 10) ~sh f =
  let size = Lwd.var (w, h) in
  let body = f (Lwd.get size) in
  body
  |> Lwd.map ~f:(fun ui ->
    ui
    |> Ui.resize ~w ~sw ~h ~sh
    |> Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then Lwd.set size (w, h)))
;;

let neutral_grav = Gravity.make ~h:`Neutral ~v:`Neutral
let make_even num = num + (num mod 2 * 1)
let upPipe = Uchar.of_int 0x2503
let tlPipe = Uchar.of_int 0x250f
let trPipe = Uchar.of_int 0x2513
let blPipe = Uchar.of_int 0x2517
let brPipe = Uchar.of_int 0x251b
let sidePipe = Uchar.of_int 0x2501

(** makes a line of chars with a specific start , midlle and end*)
let make_with_ends start mid ending width =
  Array.init width (fun i ->
    let lastIdx = width - 1 in
    match i with 0 -> start | a when a == lastIdx -> ending | _ -> mid)
  |> I.uchars A.empty
;;

let make_top width = make_with_ends tlPipe sidePipe trPipe width
let make_bot width = make_with_ends blPipe sidePipe brPipe width

(** This is for shifting something away from the edge it is pushed against *)
let pad_edge x_pad y_pad grav ui =
  let y_pad =
    match grav |> Gravity.v with
    | `Negative ->
      -y_pad
    | `Neutral ->
      0
    | `Positive ->
      y_pad
  in
  match grav |> Gravity.h with
  | `Negative ->
    ui |> Ui.shift_area (-x_pad) y_pad
  | `Neutral ->
    ui
  | `Positive ->
    ui |> Ui.shift_area x_pad y_pad
;;

let border_box ?(pad = neutral_grav) ?(pad_h = 4) ?(pad_v = 2) ?(label = "") input =
  let width = Ui.layout_width input |> make_even in
  let height = Ui.layout_height input in
  let edit =
    Ui.zcat
      [
        I.char A.empty ' ' (width + pad_h) (height + pad_v) |> Ui.atom;
        input |> Ui.resize ~pad |> pad_edge (pad_h / 2) (pad_v / 2) pad;
      ]
  in
  let p = I.uchar A.empty upPipe 1 1 in
  let v_border =
    I.vcat (List.init (edit |> Ui.layout_height) (fun _ -> p))
    |> Ui.atom
    |> Ui.resize ~pad:neutral_grav
  in
  let h_body = Ui.hcat [ v_border; edit; v_border ] in
  let width = Ui.layout_width h_body in
  let v_body =
    Ui.vcat
      [
        Ui.zcat
          [
            make_top width |> Ui.atom;
            W.string label |> Ui.resize ~pad |> pad_edge 1 0 pad;
          ];
        h_body;
        make_bot width |> Ui.atom;
      ]
  in
  v_body
;;

let grid xxs = xxs |> List.map I.hcat |> I.vcat

(** image outline creator*)
let outline_top attr w h =
  let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
  let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1 in
  let a, b = chr 0x256d, chr 0x256e in
  I.hcat [ a; hbar; b ]
;;

(** image outline creator*)
let outline_bot attr w h =
  let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
  let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1 in
  let c, d = chr 0x256f, chr 0x2570 in
  I.hcat [ d; hbar; c ]
;;

(*This is it! this box has a max width that actually works and is adheared to*)
let border_box2 ?mw ?(pad = neutral_grav) ?(pad_w = 2) ?(pad_h = 1) ?(label = "") input =
  let size = Lwd.var (0, 0) in
  let input =
    input
    |>$ Ui.size_sensor (fun ~w ~h -> if Lwd.peek size <> (w, h) then Lwd.set size (w, h))
  in
  let$* o_w, o_h = Lwd.get size in
  let w,h=(o_w+pad_w*2,o_h+pad_h*2) in
  let h = h in
  let$ input = input in
  let vbar = I.uchar A.empty (Uchar.of_int 0x2502) 1 h in
  let bbox =
    Ui.vcat
      [
        outline_top A.empty w 1 |> Ui.atom |> Ui.resize ~w:0;

        Ui.hcat [ vbar |> Ui.atom; I.void  pad_w 1|>Ui.atom;  input |> Ui.resize ~pad ;I.void  pad_w 1|>Ui.atom;  vbar |> Ui.atom ];
        outline_bot A.empty w 1 |> Ui.atom |> Ui.resize ~w:0;
      ]
  in
  bbox |> Ui.resize ?mw
;;

(** image outline creator*)
let outline attr w h =
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
  and vbar = I.uchar attr (Uchar.of_int 0x2502) 1 h in
  let a, b, c, d = chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570 in
  let void = I.void w h in
  grid [ [ a; hbar; b ]; [ vbar; void; vbar ]; [ d; hbar; c ] ]
;;

(** image outline creator*)
let label_outline ~label ~label_bottom attr i =
  let w, h = I.(width i, height i) in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and top_label = label |> I.hpad 1 0
  and bot_label = label_bottom |> I.hpad (w - (label_bottom |> I.width |> ( + ) 1)) 0
  and hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
  and vbar = I.uchar attr (Uchar.of_int 0x2502) 1 h in
  (*now we layer the bars with our labels ontop*)
  let hbar_top = [ top_label; hbar ] |> I.zcat
  and hbar_bot = [ bot_label; hbar ] |> I.zcat in
  let tl, tr, br, bl = chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570 in
  grid [ [ tl; hbar_top; tr ]; [ vbar; i; vbar ]; [ bl; hbar_bot; br ] ]
;;

let label_outline_strings ~label ~label_bottom ?(attr = A.empty) i =
  label_outline
    ~label:(I.string attr label)
    ~label_bottom:(I.string attr label_bottom)
    attr
    i
;;

(** ui outline creator*)
let ui_outline
  ?(pad = neutral_grav)
  ?(pad_w = 4)
  ?(pad_h = 2)
  ?(attr = A.empty)
  ?(label = "")
  ?(label_bottom = "")
  ui
  =
  dynamic_size ~sw:1 ~sh:1 @@ fun size ->
  let$ w, h = size in
  let outline =
    label_outline_strings
      ~label_bottom
      ~label
      ~attr
      (I.char A.empty ' ' (w + pad_w) (h + pad_h))
    |> Ui.atom
  in
  Ui.zcat [ outline; ui |> Ui.resize ~sw:0 ~pad |> pad_edge (pad_w / 2) (pad_h / 2) pad ]
;;

(** ui outline creator*)
let ui_outline2
  ?(pad = neutral_grav)
  ?(pad_w = 4)
  ?(pad_h = 2)
  ?(attr = A.empty)
  ?(label = "")
  ?(label_bottom = "")
  ui
  =
  let$* ui = ui in
  dynamic_width ~sw:1 ~sh:1 ~mw:((ui |> Ui.layout_width) + 9) @@ fun size ->
  let$ w = size in
  ui |> Ui.resize ~w ~mw:((ui |> Ui.layout_width) - 9) |> border_box
;;

(** ui outline creator*)
let ui_outline3
  ?(pad = neutral_grav)
  ?(pad_w = 4)
  ?(pad_h = 2)
  ?(attr = A.empty)
  ?(label = "")
  ?(label_bottom = "")
  ui
  =
  let$* ui = ui in
  dynamic_width ~sw:1 ~sh:1 @@ fun size ->
  let$ w = size in
  let max = (ui |> Ui.layout_width) - 5 in
  let w = if w > max then max else w in
  ui |> Ui.resize ~pad ~sw:0 ~w:(w - pad_w - 5) |> border_box
;;

let monitor ui =
  ui |> Ui.layout_spec |> Ui.pp_layout_spec Format.str_formatter;
  let str = Format.flush_str_formatter () in
  let log =
    str |> String.split_on_char '\n' |> List.map (I.string A.empty) |> I.vcat |> Ui.atom
  in
  Ui.vcat [ ui; log ]
;;

(*========Prompt=======*)
let prompt onExit name =
  let prompt_input = Lwd.var ("", 0) in
  let prompt_val = Lwd.get prompt_input in
  let$* prompt_field =
    W.zbox
      [
        W.string ~attr:A.(st underline) "                                       "
        |> Lwd.pure;
        W.edit_field
          prompt_val
          ~on_change:(fun state -> Lwd.set prompt_input state)
          ~on_submit:(fun (str, _) -> onExit (`Finished str));
      ]
  in
  let$ prompt_val, _ = prompt_val in
  prompt_field
  |> ui_outline
       ~label:name
       ~label_bottom:
         (prompt_val |> String.length |> Int.to_string |> Printf.sprintf " %s ")
  |>$ Ui.event_filter (fun event ->
    match event with
    | `Key (`Escape, _) ->
      onExit `Closed;
      `Handled
    | _ ->
      `Unhandled)
;;

(*TODO: I should check that focus handle isn't being made eadh time*)
(**This prompt will either *)
let general_prompt ?(focus = Focus.make ()) ?(char_count = false) ~show_prompt_var ui =
  let prompt_input = Lwd.var ("", 0) in
  let prompt_val = Lwd.get prompt_input in
  (*Build the ui so that it is either the prompt or nothing depending on whether show prompt is enabled*)
  let prompt_ui =
    let$* show_prompt_val = Lwd.get show_prompt_var in
    let prompt_ui =
      show_prompt_val
      |> Option.map @@ fun (label, pre_fill, on_exit) ->
         let on_exit result =
           Focus.release focus;
           show_prompt_var $= None;
           prompt_input $= ("", 0);
           on_exit result
         in
         (*we need focus because the base ui is rendering first and so *)
         Focus.request focus;
         (*prefill the prompt if we want to *)
         if prompt_input |> Lwd.peek |> fst == ""
         then prompt_input $= (pre_fill, pre_fill |> String.length);
         let$* prompt_field =
           W.zbox
             [
               W.string ~attr:A.(st underline) "                                       "
               |> Lwd.pure;
               W.edit_field
                 prompt_val
                 ~on_change:(fun state -> Lwd.set prompt_input state)
                 ~on_submit:(fun (str, _) -> on_exit (`Finished str));
             ]
         in
         let$* prompt_val, _ = prompt_val in
         let$* focus = focus |> Focus.status in
         let label_bottom =
           if char_count
           then
             Some (prompt_val |> String.length |> Int.to_string |> Printf.sprintf " %s ")
           else None
         in
         prompt_field
         |> ui_outline ~label ?label_bottom
         |>$ Ui.event_filter ~focus (fun event ->
           match event with
           | `Key (`Escape, _) ->
             on_exit `Closed;
             `Handled
           | _ ->
             `Unhandled)
    in
    prompt_ui |> Option.value ~default:(Ui.empty |> Lwd.pure)
  in
  (*Now that we have the prompt ui we layer it ontop of the normal ui using zbox.
    My hope is that by not directly nesting them this will allow the ui to not re-render when the prompt appears*)
  W.zbox [ ui; prompt_ui |> Lwd.map ~f:(Ui.resize ~pad:neutral_grav) ]
;;

(**This is a simple popup that can show ontop of *)
let popup ~show_popup_var ui =
  let popup_ui =
    let$* show_popup = Lwd.get show_popup_var in
    match show_popup with
    | Some (content, label) ->
      let prompt_field = content in
      prompt_field |> Lwd.bind ~f:(ui_outline ~label)
    | None ->
      Ui.empty |> Lwd.pure
  in
  W.zbox [ ui; popup_ui |> Lwd.map ~f:(Ui.resize ~w:25 ~pad:neutral_grav) ]
;;

let prompt_example =
  let show_prompt_var = Lwd.var None in
  let ui =
    Ui.vcat
      [
        W.string "hi this is my main ui";
        W.string "another line";
        W.string "another line";
        W.string
          "another linanother \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrste";
        W.string "another line";
        W.string
          "another linanother \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrste";
        W.string "another line";
        W.string "another line";
        W.string "another line";
        W.string "another line";
        W.string "another line";
        W.string
          "another \
           lineaorsietnaoiresntoiaernstoieanrstoiaernstoiearnostieanroseitnaoriestnoairesntoiaernsotieanrsotienaoriestnoairesntoiearnstoieanrst";
      ]
    |> Ui.keyboard_area (fun x ->
      match x with
      | `ASCII 'p', _ ->
        Lwd.set show_prompt_var @@ Some ("hi prompt", "pre_fill", fun _ -> ());
        `Handled
      | _ ->
        `Unhandled)
    |> Lwd.pure
  in
  let prompt = general_prompt ~show_prompt_var ui in
  W.h_pane prompt (W.string "other side" |> Lwd.pure)
;;

(** horizontal rule, has no width by default but is very very wide so it should fill any space*)
let h_rule =
  W.string
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  |> Ui.resize ~w:0 ~sw:100
;;

let scrollable ui =
  let scrollState = Lwd.var W.default_scroll_state in
  W.vscroll_area
    ~change:(fun _action state -> scrollState $= state)
    ~state:(Lwd.get scrollState)
    ui
;;

(*taken from citty*)

open Nottui

let empty = Lwd.pure Ui.empty

(* Find the index of next control character in a string *)
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
;;

(** Typeset the strings stored in a table with a fixed width, wrapping
    characters at the end of the line. *)
let word_wrap_string_table table width =
  if width <= 0
  then empty
  else (
    (* Wrap at least around 8 characters *)
    let width = max 8 width in
    (* Split lines around newline characters.
       Control characters will normally be \n or \r, but other might sneak in.
       TODO: allow customization of handling of control characters? *)
    let rec split_lines x acc i =
      match control_character_index x i with
      | exception Not_found ->
        String.sub x i (String.length x - i) :: acc
      | j ->
        split_lines x (String.sub x i (j - i) :: acc) (j + 1)
    in
    (* Turn an input line into visual lines.
       Split the line into chunks of the right width,
       surrounding the splits with ↳ and ↲. *)
    let wrap_line str =
      let lines = ref [] in
      let pos = ref 0 in
      let len = String.length str in
      (* Compute visual lines *)
      while len - !pos > if !pos > 0 then width - 1 else width do
        if !pos = 0
        then (
          lines := (String.sub str !pos (width - 1) ^ "↲") :: !lines;
          pos := !pos + (width - 1))
        else (
          lines := ("↳" ^ String.sub str !pos (width - 2) ^ "↲") :: !lines;
          pos := !pos + (width - 2))
      done;
      (* Produce an image for one visual line *)
      let render_line str = Ui.atom Notty.(I.string A.empty str) in
      match !lines with
      | [] ->
        (* Nothing to split, render the full input *)
        render_line str
      | lines ->
        (* Something was split:
           - append remaining characters
           - render each line
           - concatenate them vertically *)
        ("↳" ^ String.sub str !pos (len - !pos)) :: lines
        |> List.rev_map render_line
        |> Ui.vcat
    in
    (* Stack three images vertically *)
    let join3 a b c = Ui.join_y a (Ui.join_y b c) in
    (* Map reduce the table, lifting the strings to an intermediate type
       that is suitable for word wrapping.

       The input is provided as a stream of strings, but they don't represent
       lines, just a continuous stream of characters.
       Therefore we don't know where a line starts or ends until we see a
       control character.

       So individual pieces are represented as values of type
       [string * (ui * string) option]
       as follow:
       - a string "foo" that does not contain any newline character is
         [("foo", None)]
       - a string "foo\nbar" that contains a single newline character is
         [("foo", Some (empty, "bar"))]
       - a string "foo\nbar\nbaz" that contains two newline characters, and
         thus one fully defined line, is
         [("foo", Some (I.string "bar", "baz"))]
       - a string "foo\nbar\nbaz\foo" that contains three newline characters,
         and two fully defined lines, is
         [("foo", Some (I.vcat [string "bar"; string "baz"], "foo"))]

       The informal interpretation is thus [(prefix, Some (body, suffix))]:
       - [prefix] is string of character to append to the preceding line
       - [body] is the image of lines already rendered
       - [suffix] is string of character to prepend to the following line

       This type can be given monoid structure compatible with the string
       monoid. This gives a wrapping algorithm with efficient concatenation,
       suitable for incrementally rendering a stream of characters.
    *)
    Lwd_table.map_reduce
      (fun _ x ->
        match control_character_index x 0 with
        | exception Not_found ->
          x, None
        | i ->
          let prefix = String.sub x 0 i in
          (match split_lines x [] (i + 1) with
           | [] ->
             assert false
           | suffix :: rest ->
             let ui = rest |> List.rev_map wrap_line |> Lwd_utils.reduce Ui.pack_y in
             prefix, Some (ui, suffix)))
      ( ("", None),
        fun (pa, ta) (pb, tb) ->
          match ta with
          | None ->
            pa ^ pb, tb
          | Some (ua, sa) ->
            let line = sa ^ pb in
            ( pa,
              Some
                (match tb with
                 | None ->
                   ua, line
                 | Some (ub, sb) ->
                   join3 ua (wrap_line line) ub, sb) ) )
      table
    |> (* After reducing the table, we produce the final UI, interpreting
          unterminated prefix and suffix has line of their own. *)
    Lwd.map ~f:(function
      | pa, None ->
        wrap_line pa
      | pa, Some (ub, sb) ->
        join3 (wrap_line pa) ub (wrap_line sb)))
;;

(* Grab the mouse and repeat an event until button is released *)
let grab_and_repeat ~sw f =
  let stop = ref false in
  let rec step delay () =
    if not !stop
    then (
      f ();
      (*TODO: this should not be a clock*)
      Eio_unix.sleep delay;
      step 0.025 ())
    else ()
  in
  Eio.Fiber.fork ~sw (step 0.4);
  `Grab ((fun ~x:_ ~y:_ -> ()), fun ~x:_ ~y:_ -> stop := true)
;;

let on_click f ~x:_ ~y:_ = function
  | `Left ->
    f ();
    `Handled
  | _ ->
    `Unhandled
;;

let button attr text f = Ui.mouse_area (on_click f) (Nottui_widgets.string ~attr text)

(* Render a vertical scroll representing a [Nottui_widgets.scroll_state].
   The [set_scroll] function is called when the state should be updated to
   reflect a user interaction. *)
let vertical_scrollbar ~set_scroll (st : Nottui_widgets.scroll_state) =
  Eio.Switch.run @@ fun sw ->
  let bar color h = Notty.(I.char A.(bg color) ' ' 1 h) in
  let gray = Notty.A.gray 1 in
  let lightgray = Notty.A.white in
  if st.visible = 0
  then Ui.atom Notty.I.empty
  else if st.total > st.visible
  then (
    (* Compute size of the handle inside the bar *)
    let ratio = max 1 (st.visible * st.visible / st.total) in
    let rest = st.visible - ratio in
    let prefix = rest * st.position / st.bound in
    let suffix = rest - prefix in
    (* React to mouse events on the scroll bar *)
    let mouse_handler ~x:_ ~y = function
      | `Left ->
        if y < prefix
        then (
          let position = ref st.position in
          grab_and_repeat ~sw (fun () ->
            position := max 0 (!position - (st.visible / 2));
            set_scroll { st with position = !position };
            ()))
        else if y > prefix + ratio
        then (
          let position = ref st.position in
          grab_and_repeat ~sw (fun () ->
            position := min st.bound (!position + (st.visible / 2));
            set_scroll { st with position = !position };
            ()))
        else
          `Grab
            ( (fun ~x:_ ~y:y' ->
                let dy = y' - y in
                let position =
                  float st.position +. (float dy /. float st.visible *. float st.total)
                in
                let position = max 0 (min st.bound (int_of_float position)) in
                set_scroll { st with position }),
              fun ~x:_ ~y:_ -> () )
      | _ ->
        `Unhandled
    in
    Notty.I.vcat [ bar gray prefix; bar lightgray ratio; bar gray suffix ]
    |> Ui.atom
    |> Ui.mouse_area mouse_handler)
  else Ui.atom (bar gray st.visible)
;;

let list_box ~items ~render ~select =
  let prev_highlight = ref (Lwd.var false) in
  let select_item (var, item) =
    Lwd.set !prev_highlight false;
    Lwd.set var true;
    prev_highlight := var;
    select item
  in
  let select_next list =
    let rec seek = function
      | [] ->
        false
      | ((x, _), _) :: (item, _) :: _ when Lwd.peek x ->
        select_item item;
        true
      | _ :: rest ->
        seek rest
    in
    if seek list
    then ()
    else (match list with (item, _) :: _ -> select_item item | [] -> ())
  and select_prev list =
    let rec seek = function
      | [] ->
        ()
      | (item, _) :: ((y, _), _) :: _ when Lwd.peek y ->
        select_item item
      | [ (item, _) ] ->
        select_item item
      | _ :: rest ->
        seek rest
    in
    seek list
  and activate list =
    let rec seek = function
      | [] ->
        false
      | (item, _) :: _ when Lwd.peek (fst item) ->
        select_item item;
        true
      | _ :: rest ->
        seek rest
    in
    if seek list
    then ()
    else (match list with (item, _) :: _ -> select_item item | [] -> ())
  in
  let show_item x =
    let item = Lwd.var false, x in
    let ui =
      Lwd.map
        (Lwd.get (fst item))
        ~f:(fun highlight ->
          Ui.mouse_area
            (on_click @@ fun () -> select_item item)
            (render (snd item) highlight))
    in
    item, ui
  in
  let items = List.map show_item items in
  let view = Lwd_utils.pack Ui.pack_y (List.map snd items) in
  let dispatch = function
    | `Select_prev ->
      select_prev items
    | `Select_next ->
      select_next items
    | `Activate ->
      activate items
  in
  view, dispatch
;;

let fit_string str len =
  let len0 = String.length str in
  if len < len0
  then String.sub str 0 len
  else if len > len0
  then str ^ String.make (len - len0) ' '
  else str
;;

module Pane : sig
  type 'a t
  type 'a view

  val make : unit -> 'a t
  val render : 'a t -> ui Lwd.t
  val current_view : 'a t -> [ `Left | `Middle | `Right ] -> 'a view option
  val open_root : 'a t -> 'a view
  val open_subview : 'a view -> 'a view
  val close_subview : 'a view -> unit
  val set : 'a view -> 'a option -> ui Lwd.t -> unit
  val get : 'a view -> 'a option
end = struct
  type 'a visual_pane = {
    var : ui Lwd.t Lwd.var;
    mutable view : 'a view option;
  }

  and 'a view = {
    t : 'a t;
    content : ui Lwd.t Lwd.var;
    mutable tag : 'a option;
    previous : 'a view option;
  }

  and 'a t = {
    left : 'a visual_pane;
    middle : 'a visual_pane;
    right : 'a visual_pane;
  }

  let bind_pane visual view =
    visual.view <- view;
    Lwd.set
      visual.var
      (match view with None -> empty | Some view -> Lwd.join (Lwd.get view.content))
  ;;

  let make () =
    let visual () = { var = Lwd.var empty; view = None } in
    { left = visual (); middle = visual (); right = visual () }
  ;;

  let render t =
    let place_ui_var ?sw v = Lwd.(v |> get |> join |> map ~f:(Ui.resize ~w:0 ?sw)) in
    let spacer = Ui.empty |> Ui.resize ~w:1 ~sh:1 ~bg:Notty.A.(bg (gray 1)) |> Lwd.pure in
    Lwd_utils.pack
      Ui.pack_x
      [
        place_ui_var t.left.var ~sw:1;
        spacer;
        place_ui_var t.middle.var ~sw:2;
        spacer;
        place_ui_var t.right.var ~sw:6;
      ]
  ;;

  let current_view t = function
    | `Left ->
      t.left.view
    | `Middle ->
      t.middle.view
    | `Right ->
      t.right.view
  ;;

  let display view =
    let left, middle, right =
      match view with
      | { previous = None; _ } as middle ->
        None, middle, None
      | { previous = Some ({ previous; _ } as middle); _ } ->
        previous, middle, Some view
    in
    bind_pane view.t.left left;
    bind_pane view.t.middle (Some middle);
    bind_pane view.t.right right
  ;;

  let mkview t previous = { t; content = Lwd.var empty; tag = None; previous }

  let open_root t =
    let view = mkview t None in
    display view;
    view
  ;;

  let open_subview v =
    let view = mkview v.t (Some v) in
    display view;
    view
  ;;

  let close_subview v = display v

  let set view tag ui =
    view.tag <- tag;
    Lwd.set view.content ui
  ;;

  let get view = view.tag
end

(** Widgets that can adjust their contents based on the visible size *)
(* let dynamic_width_max ~mw ?(w = 0) ~sw ?h ?sh f = *)
(* let width = Lwd.var w in *)
(* let body = f (Lwd.get width) in *)
(* body *)
(* |> Lwd.map ~f:(fun ui -> *)
(* ui *)
(* |> Ui.resize ~w ~sw ?h ?sh *)
(* |> Ui.size_sensor (fun ~w ~h:_ -> if Lwd.peek width <> w then Lwd.set width w)) *)
(* ;; *)
