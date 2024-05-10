open Global_vars
open Lwd_infix
open Jj_process.Make (Global_vars.Vars)

let colored_string = Jj_tui.AnsiReverse.colored_string

let on_change () =
  let res = jj_no_log [ "show" ] |> colored_string in
  Vars.ui_state.jj_show $= res;
  let res = jj_no_log [] |> colored_string in
  Vars.ui_state.jj_tree $= res
;;
