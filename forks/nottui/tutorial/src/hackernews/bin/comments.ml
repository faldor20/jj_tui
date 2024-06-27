open! Nottui
open! Notty
open! Hackernews_api
open! Lwd_infix

(* Remeber to try to split your ui components into small helper functions so they can be easily composed*)
let make_comment_ui_attr ~attr (comment : comment Lwd.t) =
  let comment_content =
    let$ { by; text; kids; _ } = comment in
    Ui.vcat [ W.string text; W.fmt "by: %s replies: %d" by (kids |> List.length) ]
  in
  W.vbox [ comment_content |> W.Box.with_border_attr attr ]
;;

let make_comment_ui ?(focus = Focus.make ()) (comment : comment Lwd.t) =
  make_comment_ui_attr
    ~attr:
      (focus
       |> Focus.status
       |>$ fun focus -> if Focus.has_focus focus then A.(fg blue) else A.empty)
    comment
;;

let comment_children_view ?(focus = Focus.make ()) comments_view_state =
  (* Make a view for a parent component*)
  let parent_ui =
    let$* state = Lwd.get comments_view_state in
    state
    |> List.hd
    |> fst
    |> Option.map (fun x -> make_comment_ui (x |> Lwd.pure))
    |> Option.value ~default:(Ui.empty |> Lwd.pure)
  in
  (*Render all the children*)
  let children_ui =
    let items_ui =
      let$ state = Lwd.get comments_view_state in
      state
      |> List.hd
      |> snd
      |> List.map (fun x ->
        W.Lists.
          { data = x
          ; ui =
              W.Lists.selectable_item_lwd
                (W.hbox [ W.string "--" |> Lwd.pure; make_comment_ui (x |> Lwd.pure) ])
          })
    in
    (*Handle keyboard events*)
    items_ui
    |> W.Lists.selection_list_custom ~focus ~custom_handler:(fun item key ->
      match key with
      | `Enter, [] ->
        if item.data.kids |> List.length > 0
        then
          (*Because we are rendering a tree of comments we will push this new comment and it's children to the head of the list,
            that way if we want to exit we just drop the head to go up the tree of comments we have navigated through*)
          comments_view_state
          |> Lwd.update (fun x ->
            ( Some item.data
            , item.data.kids
              |> List.map (Hackernews_api.generate_fake_comment item.data.id) )
            :: x);
        `Handled
      | _ -> `Unhandled)
  in
  (*Render all the children below the parent*)
  W.vbox [ parent_ui; children_ui ]
  |>$ Ui.resize ~sw:1 ~mw:10000
  |> W.Box.focusable ~focus ~on_key:(function
    | `Escape, [] ->
      let view_state = Lwd.peek comments_view_state in
      (*If we are at least one comment deep, we should just go back to the parent comment view state*)
      if view_state |> List.length > 1
      then comments_view_state $= (view_state |> List.tl)
      else Focus.release_reversable focus;
      `Handled
    | _ -> `Unhandled)
;;

let comments_view ?(focus = Focus.make ()) (post : post option Lwd.t) : ui Lwd.t =
  let$* post = post in
  match post with
  | None -> Ui.empty |> Lwd.pure
  | Some post ->
    let children = post.kids |> List.map (Hackernews_api.generate_fake_comment post.id) in
    (*We are using a list instead of just one variable becasue we want to be able to "undo" if we open a child comment.
      Opening a child comment means appending that comment to the list and going back to it's parent means removing the head of the list*)
    let comment_var = [ None, children ] |> Lwd.var in
    comment_var |> comment_children_view ~focus
;;
