open Lwd.Infix

open Notty
open Nottui_main
include Shared

(**Original widgets that came with*)

type pane_state =
  | Split of
      { pos : int
      ; max : int
      }
  | Re_split of
      { pos : int
      ; max : int
      ; at : int
      }

let h_pane ?(splitter_color = A.lightyellow) left right =
  let state_var = Lwd.var (Split { pos = 5; max = 10 }) in
  let render state (l, r) =
    let (Split { pos; max } | Re_split { pos; max; _ }) = state in
    (*make sure the panes can get infinetly wide and shrink infinitely small*)
    let l = Ui.resize ~w:0 ~h:0 ~sh:1 ~sw:pos ~mw:1000 ~mh:1000 l in
    let r = Ui.resize ~w:0 ~h:0 ~sh:1 ~sw:(max - pos) ~mw:1000 ~mh:1000 r in
    let splitter =
      Ui.resize ~bg:Notty.A.(bg splitter_color) ~w:1 ~h:0 ~sw:0 ~sh:1 Ui.empty
    in
    let splitter =
      Ui.mouse_area
        (fun ~x:_ ~y:_ -> function
          | `Left ->
            `Grab
              ( (fun ~x ~y:_ ->
                  match Lwd.peek state_var with
                  | Split { pos; max } ->
                    Lwd.set state_var (Re_split { pos; max; at = x })
                  | Re_split { pos; max; at } ->
                    if at <> x then Lwd.set state_var (Re_split { pos; max; at = x }))
              , fun ~x:_ ~y:_ -> () )
          | _ -> `Unhandled)
        splitter
    in
    let ui = Ui.join_x l (Ui.join_x splitter r) in
    let ui = Ui.resize ~w:10 ~h:10 ~sw:1 ~sh:1 ~mh:1000 ~mw:1000 ui in
    let ui =
      match state with
      | Split _ -> ui
      | Re_split { at; _ } ->
        Ui.transient_sensor
          (fun ~x ~y:_ ~w ~h:_ () ->
            let newpos = clampi (at - x) ~min:0 ~max:w in
            Lwd.set state_var (Split { pos = newpos; max = w }))
          ui
    in
    ui
  in
  Lwd.map2 ~f:render (Lwd.get state_var) (Lwd.pair left right)
;;

let v_pane top bot =
  let state_var = Lwd.var (Split { pos = 5; max = 10 }) in
  let render state (top, bot) =
    let (Split { pos; max } | Re_split { pos; max; _ }) = state in
    let top = Ui.resize ~w:0 ~h:0 ~sw:1 ~sh:pos top in
    let bot = Ui.resize ~w:0 ~h:0 ~sw:1 ~sh:(max - pos) bot in
    let splitter =
      Ui.resize ~bg:Notty.A.(bg lightyellow) ~w:0 ~h:1 ~sw:1 ~sh:0 Ui.empty
    in
    let splitter =
      Ui.mouse_area
        (fun ~x:_ ~y:_ -> function
          | `Left ->
            `Grab
              ( (fun ~x:_ ~y ->
                  match Lwd.peek state_var with
                  | Split { pos; max } ->
                    Lwd.set state_var (Re_split { pos; max; at = y })
                  | Re_split { pos; max; at } ->
                    if at <> y then Lwd.set state_var (Re_split { pos; max; at = y }))
              , fun ~x:_ ~y:_ -> () )
          | _ -> `Unhandled)
        splitter
    in
    let ui = Ui.join_y top (Ui.join_y splitter bot) in
    let ui = Ui.resize ~w:10 ~h:10 ~sw:1 ~sh:1 ui in
    let ui =
      match state with
      | Split _ -> ui
      | Re_split { at; _ } ->
        Ui.transient_sensor
          (fun ~x:_ ~y ~w:_ ~h () ->
            let newpos = clampi (at - y) ~min:0 ~max:h in
            Lwd.set state_var (Split { pos = newpos; max = h }))
          ui
    in
    ui
  in
  Lwd.map2 ~f:render (Lwd.get state_var) (Lwd.pair top bot)
;;

let sub' str p l = if p = 0 && l = String.length str then str else String.sub str p l

let edit_field ?(focus = Focus.make ()) state ~on_change ~on_submit =
  let update focus_h focus (text, pos) =
    let pos = clampi pos ~min:0 ~max:(String.length text) in
    let content =
      Ui.atom
      @@ I.hcat
      @@
      if Focus.has_focus focus
      then (
        let attr = attr_clickable in
        let len = String.length text in
        (if pos >= len
         then [ I.string attr text ]
         else [ I.string attr (sub' text 0 pos) ])
        @
        if pos < String.length text
        then
          [ I.string A.(bg lightred) (sub' text pos 1)
          ; I.string attr (sub' text (pos + 1) (len - pos - 1))
          ]
        else [ I.string A.(bg lightred) " " ])
      else [ I.string A.(st underline) (if text = "" then " " else text) ]
    in
    let handler = function
      | `ASCII 'U', [ `Ctrl ] ->
        on_change ("", 0);
        `Handled (* clear *)
      | `Escape, [] ->
        Focus.release focus_h;
        `Handled
      | `ASCII k, _ ->
        let text =
          if pos < String.length text
          then
            String.sub text 0 pos
            ^ String.make 1 k
            ^ String.sub text pos (String.length text - pos)
          else text ^ String.make 1 k
        in
        on_change (text, pos + 1);
        `Handled
      | `Backspace, _ ->
        let text =
          if pos > 0
          then
            if pos < String.length text
            then
              String.sub text 0 (pos - 1) ^ String.sub text pos (String.length text - pos)
            else if String.length text > 0
            then String.sub text 0 (String.length text - 1)
            else text
          else text
        in
        let pos = maxi 0 (pos - 1) in
        on_change (text, pos);
        `Handled
      | `Enter, _ ->
        on_submit (text, pos);
        `Handled
      | `Arrow `Left, [] ->
        let pos = mini (String.length text) pos in
        if pos > 0
        then (
          on_change (text, pos - 1);
          `Handled)
        else `Unhandled
      | `Arrow `Right, [] ->
        let pos = pos + 1 in
        if pos <= String.length text
        then (
          on_change (text, pos);
          `Handled)
        else `Unhandled
      | _ -> `Unhandled
    in
    Ui.keyboard_area ~focus handler content
  in
  let node = Lwd.map2 ~f:(update focus) (Focus.status focus) state in
  let mouse_grab (text, pos) ~x ~y:_ = function
    | `Left ->
      if x <> pos then on_change (text, x);
      Focus.request focus;
      `Handled
    | _ -> `Unhandled
  in
  Lwd.map2 state node ~f:(fun state content -> Ui.mouse_area (mouse_grab state) content)
;;



(** Prints the summary, but calls [f()] to compute a sub-widget
    when clicked on. Useful for displaying deep trees. Mouse only *)
let unfoldable ?(folded_by_default = true) summary (f : unit -> Ui.t Lwd.t) : Ui.t Lwd.t =
  let open Lwd.Infix in
  let opened = Lwd.var (not folded_by_default) in
  let fold_content =
    Lwd.get opened
    >>= function
    | true ->
      (* call [f] and pad a bit *)
      f () |> Lwd.map ~f:(Ui.join_x (string " "))
    | false -> empty_lwd
  in
  (* pad summary with a "> " when it's opened *)
  let summary =
    Lwd.get opened
    >>= fun op ->
    summary
    >|= fun s ->
    Ui.hcat [ string ~attr:attr_clickable (if op then "v" else ">"); string " "; s ]
  in
  let cursor ~x:_ ~y:_ = function
    | `Left when Lwd.peek opened ->
      Lwd.set opened false;
      `Handled
    | `Left ->
      Lwd.set opened true;
      `Handled
    | _ -> `Unhandled
  in
  let mouse = Lwd.map ~f:(fun m -> Ui.mouse_area cursor m) summary in
  Lwd.map2 mouse fold_content ~f:(fun summary fold ->
    (* TODO: make this configurable/optional *)
    (* newline if it's too big to fit on one line nicely *)
    let spec_sum = Ui.layout_spec summary in
    let spec_fold = Ui.layout_spec fold in
    (* TODO: somehow, probe for available width here? *)
    let too_big =
      spec_fold.Ui.h > 1 || (spec_fold.Ui.h > 0 && spec_sum.Ui.w + spec_fold.Ui.w > 60)
    in
    if too_big
    then Ui.join_y summary (Ui.join_x (string " ") fold)
    else Ui.join_x summary fold)
;;



(** A grid layout, with alignment in all rows/columns.
    @param max_h maximum height of a cell
    @param max_w maximum width of a cell
    @param bg attribute for controlling background style
    @param h_space horizontal space between each cell in a row
    @param v_space vertical space between each row
    @param pad used to control padding of cells
    @param crop
      used to control cropping of cells
      TODO: control padding/alignment, vertically and horizontally
      TODO: control align left/right in cells
      TODO: horizontal rule below headers
      TODO: headers *)
let grid
  ?max_h
  ?max_w
  ?pad
  ?crop
  ?bg
  ?(h_space = 0)
  ?(v_space = 0)
  ?(headers : Ui.t Lwd.t list option)
  (rows : Ui.t Lwd.t list list)
  : Ui.t Lwd.t
  =
  let rows =
    match headers with
    | None -> rows
    | Some r -> r :: rows
  in
  (* build a [ui list list Lwd.t] *)
  Lwd_utils.map_l (fun r -> Lwd_utils.flatten_l r) rows
  >>= fun (rows : Ui.t list list) ->
  (* determine width of each column and height of each row *)
  let n_cols = List.fold_left (fun n r -> maxi n (List.length r)) 0 rows in
  let col_widths = Array.make n_cols 1 in
  List.iter
    (fun row ->
      List.iteri
        (fun col_j cell ->
          let w = (Ui.layout_spec cell).Ui.w in
          col_widths.(col_j) <- maxi col_widths.(col_j) w)
        row)
    rows;
  (match max_w with
   | None -> ()
   | Some max_w ->
     (* limit width *)
     Array.iteri (fun i x -> col_widths.(i) <- mini x max_w) col_widths);
  (* now render, with some padding *)
  let pack_pad_x =
    if h_space <= 0
    then Ui.empty, Ui.join_x
    else Ui.empty, fun x y -> Ui.hcat [ x; Ui.space h_space 0; y ]
  and pack_pad_y =
    if v_space = 0
    then Ui.empty, Ui.join_y
    else Ui.empty, fun x y -> Ui.vcat [ x; Ui.space v_space 0; y ]
  in
  let rows =
    List.map
      (fun row ->
        let row_h = List.fold_left (fun n c -> maxi n (Ui.layout_spec c).Ui.h) 0 row in
        let row_h =
          match max_h with
          | None -> row_h
          | Some max_h -> mini row_h max_h
        in
        let row =
          List.mapi (fun i c -> Ui.resize ~w:col_widths.(i) ~h:row_h ?crop ?pad ?bg c) row
        in
        Lwd_utils.reduce pack_pad_x row)
      rows
  in
  (* TODO: mouse and keyboard handling *)
  let ui = Lwd_utils.reduce pack_pad_y rows in
  Lwd.return ui
;;

(** Turn the given [ui] into a clickable button, calls [f] when clicked. *)
let button_of ui f =
  Ui.mouse_area
    (fun ~x:_ ~y:_ _ ->
      f ();
      `Handled)
    ui
;;

(** A clickable button that calls [f] when clicked, labelled with a string. *)
let button ?(attr = attr_clickable) s f = button_of (string ~attr s) f


let toggle, toggle' =
  let toggle_ st (lbl : string Lwd.t) (f : bool -> unit) : Ui.t Lwd.t =
    let mk_but st_v lbl_v =
      let lbl =
        Ui.hcat
          [ printf "[%s|" lbl_v
          ; string ~attr:attr_clickable (if st_v then "✔" else "×")
          ; string "]"
          ]
      in
      button_of lbl (fun () ->
        let new_st = not st_v in
        Lwd.set st new_st;
        f new_st)
    in
    Lwd.map2 ~f:mk_but (Lwd.get st) lbl
  in
  (* Similar to {!toggle}, except it directly reflects the state of a variable. *)
  let toggle' (lbl : string Lwd.t) (v : bool Lwd.var) : Ui.t Lwd.t =
    toggle_ v lbl (Lwd.set v)
  (* a toggle, with a true/false state *)
  and toggle ?(init = false) (lbl : string Lwd.t) (f : bool -> unit) : Ui.t Lwd.t =
    let st = Lwd.var init in
    toggle_ st lbl f
  in
  toggle, toggle'
;;
