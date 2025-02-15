module Internal = struct
let normalise_os raw =
  match String.lowercase_ascii raw with "darwin" | "osx" -> "macos" | s -> s
;;
end

let poll_os () =
  let raw =
    match Sys.os_type with
    | "Unix" ->
      (try
         let uname_in  = Unix.open_process_args_in "uname" [| "uname"; "-s" |] in
         let str = uname_in |> In_channel.input_all in
         Unix.wait()|>ignore;
         Some (str |> String.lowercase_ascii |> String.trim)
       with
       | _ ->
         None)
    | s ->
      Some (s |> String.lowercase_ascii |> String.trim)
  in
  match raw with None | Some "" -> None | Some s -> Some (Internal.normalise_os s)
;;