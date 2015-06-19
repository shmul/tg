open Core.Std
open Tg

let as_eq a b = OUnit.assert_equal a b ~printer:(fun x -> sprintf "'%s'" x)
let rec eq_list a b =
  match (a,b) with
  | ([],[]) -> true
  | (hda :: tla,hdb :: tlb) ->
     if hda=hdb then
       eq_list tla tlb
     else
       false
  | _ -> false

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
  let pairs = [
      [Disctrack;Title],"08 Mountain Jam";
      [Disctrack;Title],"8. Mountain Jam";
      [Disctrack;Title],"8) Mountain Jam";
      [Rest;Disctrack],"Tedeschi Trucks Band 2013-10-17 NPR03";
      [Rest;Disc;Track;Title],"ph131230d1_02_Bathtub_Gin";
      [Rest;Disc;Track;Title],"ph131228d1_02_Stealing_Time_From_The_Faulty_Plan";
      [Disc;Track;Title],"2-03 Been So Long";
      [Rest;Disc;Track],"gd90-09-20d2t02";
      [Rest;Disc;Track],"ABB1973-12-31d4t01";
    ] in
  List.iter pairs ~f:(fun (expected, raw)->
                      match Tg.guess_fields_from_file_name raw with
                        | (expr,_) :: _ ->
                           if not (eq_list expected expr) then
                             OUnit.assert_failure (sprintf "failed on '%s'\n" raw)
                        | _ -> ())

let dates () =
  let pairs = [
      "49-12-30","12301949";
      "59-12-30","19591230";
      "69-12-30","1969-12-30";
      "79-12-30","12/30/1979";
      "89-12-30","1989 30 12";
      "99-12-30","30_12_99";
      "49-10-20","102049";
      "89-12-20","201289";
      "58-01-10","58/01/10";
      "13-12-30","ph131230d1_02_Bathtub_Gin";
      "76-06-10","gd76-06-10Berthad3t05";
      "87-10-21","jgb87-10-21d1t03";
      "70-06-04","C$NY  06-04-1970 NY (SBD)";
      "99-07-23","David Nelson Band, 7 23 99 two";
      "99-07-23","David Nelson Band, 7-23-99 two";
      "90-09-20","gd90-09-20d2t02.flac";
      "73-12-31","ABB1973-12-31d4t01";
      "95-06-02","Robert Jr. Lockwood 1995-06-02";
    ] in
  List.iter pairs ~f:(fun (expected, raw)->
                      as_eq expected (guess_date raw)
                      )

let test_set = [
    "normalize", `Quick, norm;
    "matches", `Quick, matches;
    "date", `Quick, dates;
  ]

let () =
  Alcotest.run "tg tests" [
                 "test_set",test_set;
               ]
