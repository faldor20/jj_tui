type rev_id = {
    change_id : string
  ; commit_id : string

  ; divergent : bool(** Indicates the changeid is conflicted and we must use the commitid *)
}

type 'a maybe_unique =
  | Unique of 'a
  | Duplicate of 'a
