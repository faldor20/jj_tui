let test_input=
  let inp_var =("hi there",5)|>Lwd.var in
  let inp_text= inp_var|>Lwd.get in

  W.edit_field  inp_text  ~on_change:(fun x->Lwd.set inp_var x) ~on_submit:(fun x->()) 

let test_focused_border =
  let focus = Focus.make () in
  let content = W.string "Click to focus this border" in
  content
  |> Lwd.pure
  |> Lwd.map ~f:(fun ui -> 
    ui 
    |> Ui.keyboard_area ~focus:(Focus.status focus) (fun _ -> `Unhandled)
    |> Ui.border 
        ~thick:1 
        ~pad_w:2 
        ~pad_h:1
        ~attr:(A.fg A.white)
        ~style:Ui.Border.unicode
        ~focus_attr:(A.fg A.blue)
        ~focus_style:Ui.Border.unicode_bold
    |> Ui.mouse_area (fun ~x:_ ~y:_ _ -> 
        Focus.request focus; 
        `Handled))

let w_0 =
  W.vbox
    [
      W.hbox
        [
          Ui.border ~thick:1 ~style:Ui.Border.unicode (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;
          Ui.border ~thick:1 ~style:Ui.Border.unicode_double (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;
          Ui.border ~thick:0 ~style:Ui.Border.unicode_rounded (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;

          Ui.border ~thick: 2 ~pad_w:2 ~pad_h:1 ~style:Ui.Border.unicode (Ui.vcat [W.string "hi this is a ui element with  a\n border"; W.string "hi"])|>Lwd.pure;

          W.Box.box ~pad_w:2 ~pad_h:1 (Ui.vcat [W.string "hi this is a ui element with  an\n old style border box"; W.string "hi"]|>Lwd.pure);
          (* pString " |" *)
        (* ; (let og = *)
             (* Ui.vcat *)
               (* [ *)
                 (* W.string "123456789000000000000000000000000000000000000000000000000000end" *)
               (* ; W.string "123456789000000000000000000000000000000000000000000000000000end" *)
               (* ] *)
           (* in *)
           (* og *)
           (* |> Lwd.pure *)
           (* |> W.Scroll.area *)
           (* |> W.Box.box *)
           (* |>$ Ui.resize ~sh:1 ~mh:1000 *)
           (* |> W.size_logger) *)
        (* ; pString "| " *)
        test_input|>$ Ui.border ~thick:1 ~pad_w:1 ~pad_h:1 ~style:Ui.Border.unicode
        ]
    ; W.string " " |> Lwd.pure
    ; W.string "Test focused border (click to focus):" |> Lwd.pure  
    ; test_focused_border
    ] 