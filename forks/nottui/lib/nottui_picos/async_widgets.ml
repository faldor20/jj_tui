open Notty
open Nottui
open Picos_std_structured
let throbber =
  let frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |] in
  let len = Array.length frames in
  let frame_var = Lwd.var 0 in
  
  (* let update_throb= Lwd.var 0 in *)
  Lwd.bind (Lwd.get frame_var) ~f:(fun frame ->
    (*each time we re-render, we start a new thread to update the frame the next time*)
    Picos_std_structured.Flock.fork(fun _ ->
        Unix.sleepf 0.1;
        Lwd.set frame_var (Lwd.peek frame_var + 1);
      );
    W.string ~attr:A.(fg blue) (frames.(frame mod len) )|>Lwd.pure
  )
;;
