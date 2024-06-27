open! Nottui
open! Notty

let dynamic_width ?(w = 0) ?mw ~sw ?h ?sh f =
  let width = Lwd.var w in
  let body = f (Lwd.get width) in
  body
  |> Lwd.map ~f:(fun ui ->
    ui
    |> Ui.resize ~w ~sw ?mw ?h ?sh
    |> Ui.size_sensor (fun ~w ~h:_ -> if Lwd.peek width <> w then Lwd.set width w))
;;

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
      ( ("", None)
      , fun (pa, ta) (pb, tb) ->
          match ta with
          | None ->
            pa ^ pb, tb
          | Some (ua, sa) ->
            let line = sa ^ pb in
            ( pa
            , Some
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

let button attr text f = Ui.mouse_area (on_click f) (W.string ~attr text)

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
      var : ui Lwd.t Lwd.var
    ; mutable view : 'a view option
  }

  and 'a view = {
      t : 'a t
    ; content : ui Lwd.t Lwd.var
    ; mutable tag : 'a option
    ; previous : 'a view option
  }

  and 'a t = {
      left : 'a visual_pane
    ; middle : 'a visual_pane
    ; right : 'a visual_pane
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
        place_ui_var t.left.var ~sw:1
      ; spacer
      ; place_ui_var t.middle.var ~sw:2
      ; spacer
      ; place_ui_var t.right.var ~sw:6
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
