open Core.Std

let as_eq a b = OUnit.assert_equal a b ~printer:(fun x -> sprintf "'%s'" x)

let norm () =
  let pairs = ["Hello Cruel World","hello cruel World";
               "Hello Cruel World","hello     cruel_woRld";
               "Hello Cruel World","     hello cruel world *#%";
               "","";
               "","   ";
               "Hello","hello";
               "Hello","hello%20";
               "Hello Cruel World","hello%20cr%75el%5fWor%6cd";
              ] in
  List.iter pairs ~f:(fun (expected, raw)-> as_eq expected (Tg.normalize raw))


let matches () =
  let pairs = ["%N %t","08 Mountain Jam";
               "%N %t","8. Mountain Jam";
               "%N %t","8) Mountain Jam";
               "%a %N","Tedeschi Trucks Band 2013-10-17 NPR03";
               "%a %d %n %t","ph131230d1_02_Bathtub_Gin";
               "%a %d %n %t","ph131228d1_02_Stealing_Time_From_The_Faulty_Plan";
               "%d %n %t","2-03 Been So Long.mp3";
              ] in
  List.iter pairs ~f:(fun (expected, raw)->
                      match Tg.guess_fields_from_file_name raw with
                        | (expr,_) :: tl -> as_eq expected expr
                        | _ -> ())

let test_set = [
    "normalize", `Quick, norm;
  ]

let () =
  Alcotest.run "tg tests" [
                 "test_set",test_set;
               ]
