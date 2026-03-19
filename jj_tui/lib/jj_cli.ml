(** Utilities for constructing jj command-line argument lists.

    jj is invoked via [Unix.create_process_env] (direct argv), not through a
    shell.  That means wrapping file paths in single-quotes would pass the
    quote characters as literal bytes to jj — wrong.  The POSIX end-of-options
    sentinel [--] is the correct mechanism: it tells jj that every subsequent
    argument is a file path, not a flag. *)

(** [with_files base_args paths] appends [--] followed by every non-empty
    string in [paths] to [base_args].  Empty strings are silently dropped so
    callers need not pre-filter placeholder values.  When all paths are empty
    the list is returned unchanged (no spurious [--] emitted). *)
let with_files base_args paths =
  match List.filter (fun p -> p <> "") paths with
  | [] -> base_args
  | real_paths -> base_args @ [ "--" ] @ real_paths
