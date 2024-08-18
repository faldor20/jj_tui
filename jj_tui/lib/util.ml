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
