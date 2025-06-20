include Lwd_impl.Make(Mutex_picos)

(* This should prevent the set from being cancelled and leaving hanging locks*)
let set vx x =
  Picos_std_structured.Control.protect (fun () ->
      set vx x;
  )
