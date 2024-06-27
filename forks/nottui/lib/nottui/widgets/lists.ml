open Nottui_main
open Shared
open Lwd_infix

let vlist ?(bullet = "- ") (l : Ui.t Lwd.t list) : Ui.t Lwd.t =
  l
  |> List.map (fun ui -> Lwd.map ~f:(Ui.join_x (string bullet)) ui)
  |> Lwd_utils.pack Ui.pack_y
;;

let vlist_with
  ?(bullet = "- ")
  ?(filter = Lwd.return (fun _ -> true))
  (f : 'a -> Ui.t Lwd.t)
  (l : 'a list Lwd.t)
  : Ui.t Lwd.t
  =
  let open Lwd.Infix in
  let rec filter_map_ acc f l =
    match l with
    | [] -> List.rev acc
    | x :: l' ->
      let acc' =
        match f x with
        | None -> acc
        | Some y -> y :: acc
      in
      filter_map_ acc' f l'
  in
  let l = l |>$ List.map (fun x -> x, Lwd.map ~f:(Ui.join_x (string bullet)) @@ f x) in
  let l_filter : _ list Lwd.t =
    filter
    >>= fun filter ->
    l >|= filter_map_ [] (fun (x, ui) -> if filter x then Some ui else None)
  in
  l_filter >>= vbox
;;
