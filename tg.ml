open Core.Std
module Regex = Re2.Regex


type track = {
    filename : string;
    track : int;
    album : string;
    artist : string;
    title : string;
    disc : int option;
    rest : (string, string) Hashtbl.t;
  }

(* several heuristics - d/D as prefix for disc; t/T for track;
   free text after numbers as title;
   5-8 digits - date;
   3 digits - disc and track

   %a - album
   %d - disc
   %n - track
   %N - disc/track
   %t - title
 *)
let file_name_guesses = List.map [
                  "%a %d %n %t","(.*)[dD][\\s\\._-]*(\\d)[\\s\\.\\)tT_-]+(\\d+)[\\s\\.-]*(.+)";
                  "%d %n %t","[dD][\\s\\._-]*(\\d+)[\\s\\.\\)_-]*[tT][\\s\\.-]*(\\d+)[\\s\\.-]*(.*)";
                  "%d %n %t","(\\d)[\\s\\.\\)_-]+(\\d+)[\\s\\.-]+(.*)";
                  "%a %N","(.+?)(\\d{1,3})";
                  "%a %N %t","(.+)[\\s\\.-]*(\\d{3})[\\s\\.\\)_-]+(.*)";
                  "%N %t","[\\s\\.-]*(\\d{1,3})[\\s\\.\\)_-]*(.+)";
                  "%t","(.+)";
                ] ~f:(fun (expr, re) -> ( expr,Regex.create_exn ("^"^re^"$") ))

let guess_fields_from_file_name str =
  List.fold file_name_guesses ~init:[]
            ~f:(fun seed (expr, re) ->
                match Regex.find_submatches re str with
                | Ok m -> (expr,m) :: seed
                | Error _ -> seed
               ) |> List.rev


let guess_field str =
  str


(* try to extract dates from a string, and return it in YY-MM-DD format. User input might be required.
*)
let date_delims = "\\s\\.\\_:\\-"
let date_guesses_1 = Regex.create_exn ("\\d[\\d"^date_delims^"]+")

let yy_mm_dd ?year d1 d2 d3 =
  Some ""

let guess_date_1 str =
  let digits = Regex.split (Regex.create_exn ("["^date_delims^"]+")) str in
  let sub s pos len = (String.sub ~pos:pos ~len:len s) in
  try
    match digits with
    | d1::d2::d3::[] -> yy_mm_dd d1 d2 d3
    | d1::d2::[] ->
       let yy_mm_dd_helper s t = yy_mm_dd s (sub t 0 2) (sub t 2 2) in
       if (String.length d2)=4 then
         yy_mm_dd_helper d1 d2
       else if (String.length d1)=4 then
         yy_mm_dd_helper d2 d1
       else
         None
    | d1::[] ->
       (match (String.length d1) with
       | 8 ->
          (* it can be yyyy---- or ----yyyy ,where yyyy is matched against 20yy or 19yy *)
          if Regex.matches (Regex.create_exn "(20|19)\\d{6}") d1 then
            yy_mm_dd ~year:1 (sub d1 0 4) (sub d1 4 2) (sub d1 6 2)
          else if Regex.matches (Regex.create_exn "\\d{4}(20|19)\\d{2}") d1 then
           yy_mm_dd ~year:3 (sub d1 0 2) (sub d1 2 2) (sub d1 4 4)
          else None
       | 6 -> yy_mm_dd (sub d1 0 2) (sub d1 2 2) (sub d1 4 2)
       | _ -> None)
    | _ -> None
  with Failure _ -> None

let guess_date str =
  if Regex.matches date_guesses_1 str then
    match guess_date_1 str with
    | Some x -> x
    | None -> str
  else
    str

(* drop extra spaces; capitalize; decode; remove "_#*"; *)
let split_pats = Regex.create_exn "[ _%#\\*]+"

let urldecode str = try Netencoding.Url.decode str with
                      | _ -> str

let normalize str =
  let parts = Regex.split split_pats (urldecode str) in
  String.concat ~sep:" " (List.filter parts ~f:(fun x -> (String.length x)>0) |>
                            List.map  ~f:String.lowercase |>
                            List.map ~f:String.capitalize)

(* command line handling *)
let readable_file =
  Command.Spec.Arg_type.create
    (fun fn -> In_channel.with_file fn ~f:(fun _ -> fn))

let inchan = function
  | None -> In_channel.create "/dev/null"
  | Some "-" -> In_channel.stdin
  | Some filename -> open_in filename

let guess =
  Command.basic
    ~summary: "Guess fields or dates"
    Command.Spec.(empty
      +> flag "-i" (optional string) ~doc:"value Value to process"
      +> flag "-f" no_arg ~doc:" guess field"
      +> flag "-d" no_arg ~doc:" guess date"
      +> flag "-n" no_arg ~doc:"Normalize"
		  +> anon (maybe ("file" %: readable_file)))
		(fun value field date norm file () ->
     let nop s = s in
     let func =
       if field then
         guess_field
       else if date then
         guess_date
       else if norm then
         normalize
       else
         nop in
     match value with
       | Some x -> printf "%s -> %s\n" x (func x)
       | None ->
          let ch = inchan file in
          In_channel.iter_lines ~fix_win_eol:true ch
		                            ~f:(fun line ->
                                    printf "%s -> %s\n" line (func line)
                                   );
    )

let () =
  Command.run ~version:"0.1"
			        guess
