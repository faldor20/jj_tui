type rev_id = {
    change_id : string
  ; commit_id : string
}

type 'a maybe_unique =
  | Unique of 'a
  | Duplicate of 'a
