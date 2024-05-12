open Angstrom
open Util
let parse_descriptions str =
  let true_key = string "::true::" in
  let other_key = string "::false::" in
  let ending = string "::end::" in
  let body =
    many_till (take_while (function ':' -> false | _ -> true)) ending
    <* char '\n'
    <?> "body"
  in
  let mySection = true_key *> char '\n' *> body <?> "current section" in
  let otherSection = other_key *> char '\n' *> body <?> "other section" in
  let currentSection =
    mySection >>= fun mine ->
    otherSection >>= fun prev -> return (mine, prev)
  in
  let doc = skip_many otherSection *> currentSection in
  parse_query doc str
;;

let%expect_test "str" =
  let data =
    {|::true::
hiii
::end::
::false::
heyyyy
::end::
::false::
hiiiii
::end::
::false::
  hi
hi

::end::
::false::
◉  top
@  bottom

::end::
::false::
'◉  top
@  '  bottom
│  top''

::end::
::false::
'  '  hi
  hi'
hi'

::end::
::false::
     hi there mat
     yyyyyyyy
    this is a commit
    commit
  top commit
  topper commit

::end::
::false::
  hi2
hi3

::end::
::false::
hi

::end::
::false::
      this is a thi
    this is anoth
  now anothe
  no
  this is ne

::end::
::false::
  h
  this is my message 
    hello this is my special 
    hi \n this is \n a multi  l
      
      th
│
      m

::end::
::false::
heyqqCq

::end::
::false::

::end::
::false::

::end::
::false::
message 

::end::
::false::
hello this is a box

::end::
::false::
uy;yu;yu;yu

::end::
::false::

::end::
::false::
coloured output doesn't crash

::end::
::false::
hi

hi2

::end::
::false::

::end::
|}
  in
  (match data |> parse_descriptions with
   | Ok (fst, snd) ->
     (fst |> String.concat ";") ^"]["^
     (snd |> String.concat ";") |> print_endline
   | Error e ->
     print_endline e);
  [%expect {|
    hiii
    ][heyyyy
  |}]
;;
