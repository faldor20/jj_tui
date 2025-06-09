(* Default stdlib mutex implementation, actual implementation is in lwd_impl.ml *)
include Lwd_impl.Make(Mutex_backend.Stdlib)