open Core.Std

let as_eq a b = OUnit.assert_equal a b ~printer:(fun x -> sprintf "'%s'" x)

let norm () =
  let pairs = ["Hello Cruel World","hello cruel World";
               "Hello Cruel World","hello     cruel_woRld";
               "Hello Cruel World","     hello cruel world *#%";
               "","";
               "","   ";
               "Hello","hello"
              ] in
  List.iter pairs ~f:(fun (expected, raw)-> as_eq expected (Tg.normalize raw))



let test_set = [
    "normalize", `Quick, norm;
  ]

let () =
  Alcotest.run "tg tests" [
                 "test_set",test_set;
               ]
