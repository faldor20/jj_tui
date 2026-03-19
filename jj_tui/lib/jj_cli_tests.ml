open Jj_cli

(* Display args separated by | so spaces inside an argument are visible and
   distinct from argument boundaries. *)
let show args = print_string (String.concat "|" args); print_newline ()

let%expect_test "with_files: empty path list yields no separator" =
  show (with_files [ "diff"; "-r"; "abc" ] []);
  [%expect {| diff|-r|abc |}]
;;

let%expect_test "with_files: all-empty strings yield no separator" =
  show (with_files [ "diff" ] [ ""; "" ]);
  [%expect {| diff |}]
;;

let%expect_test "with_files: single path" =
  show (with_files [ "diff"; "-r"; "abc" ] [ "src/foo.ml" ]);
  [%expect {| diff|-r|abc|--|src/foo.ml |}]
;;

let%expect_test "with_files: multiple paths remain separate argv entries" =
  show (with_files [ "restore"; "--to"; "abc" ] [ "a/b.ml"; "c/d.ml" ]);
  [%expect {| restore|--to|abc|--|a/b.ml|c/d.ml |}]
;;

let%expect_test "with_files: path with spaces is a single argv entry" =
  show (with_files [ "diff" ] [ "my file.txt" ]);
  [%expect {| diff|--|my file.txt |}]
;;

let%expect_test "with_files: path with leading dash is protected by separator" =
  show (with_files [ "diff" ] [ "-not-a-flag.txt" ]);
  [%expect {| diff|--|-not-a-flag.txt |}]
;;

let%expect_test "with_files: empty strings mixed with real paths are dropped" =
  show (with_files [ "diff" ] [ ""; "real.ml"; "" ]);
  [%expect {| diff|--|real.ml |}]
;;
