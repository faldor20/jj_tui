open Angstrom

let fail_to_string marks err = String.concat " > " marks ^ ": " ^ err

let state_to_verbose_result = function
  | Buffered.Partial _ ->
    Error "incomplete input"
  | Done (_, v) ->
    Ok v
  | Fail (unconsumed, marks, msg) ->
    let remaining_big_string =
      Bigstringaf.sub unconsumed.buf ~off:unconsumed.off ~len:unconsumed.len
    in
    let combined_msg =
      "failed '"
      ^ fail_to_string marks msg
      ^ "' with unconsumed:"
      ^ Bigstringaf.to_string remaining_big_string
      |> String.escaped
    in
    Error combined_msg
;;

(** This is a much more user friendly parse function that gives good errors*)
let parse_query parser s =
  let initial_parser_state = Buffered.parse parser in
  let final_parser_state = Buffered.feed initial_parser_state (`String s) in
  state_to_verbose_result final_parser_state
;;

Base.List.intersperse

let ( <-$ ) f v = Lwd.map ~f (Lwd.get v)
let ( $-> ) v f = Lwd.map ~f (Lwd.get v)
let ( let$$ ) v f = Lwd.map ~f (Lwd.get v)
let ( |>$ ) v f = Lwd.map ~f v
let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
let ( |>$$ ) v2 v f = Lwd.map2 ~f v v2

module String = struct
  include String

(** Concatenates any non-empty strings in the given array*)
  let concat_non_empty sep strings  =
    strings |> List.filter (Base.String.is_empty >> not) |> String.concat sep
  ;;
end
(** convinience method to take a str and turn it into a unicode char*)
let make_uchar str =
  let a = String.get_utf_8_uchar str 0 in
  if a |> Uchar.utf_decode_is_valid
  then a |> Uchar.utf_decode_uchar
  else failwith "not a unicode string"
;;

(** Takes a list and pair all elements we can*)
let rec list_to_pairs lst =
  match lst with
  | [] | [ _ ] ->
    [] (* If list is empty or has only one element, return empty list *)
  | x :: y :: rest ->
    (x, y) :: list_to_pairs rest
;;
(** Iterate over a list in pairs. f_last handles if there is a last elemment that doesn't have a pair*)
let rec pairwise f ~f_last lst =
  match lst with
  | [ single ] ->
    f_last single
  | x :: y :: rest ->
    ((x, y) |> f) :: pairwise f ~f_last rest
  | [] ->
    [] (* If list is empty or has only one element, return empty list *)
;;

