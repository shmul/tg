open Core.Std
module Regex = Re2.Regex


type tag =
  | Album
  | Artist
  | Track
  | Disc
  | Disctrack
  | Title
  | Rest
  | Year
  | Location
  | Date


let tag_of_string = function
    | "TITLE" -> Title
    | "ALBUM" -> Album
    | "TRACKNUMBER" -> Track
    | "ARTIST" -> Artist
    | _ -> Rest

let string_of_tag = function
  | Album -> "Album"
  | Artist -> "Artist"
  | Track -> "Track"
  | Disc -> "Disc"
  | Disctrack -> "Disctrack"
  | Title -> "Title"
  | Rest -> "Rest"
  | Year -> "Year"
  | Location -> "Location"
  | Date -> "Date"

let single_file_tags fname =
  let open Core.Std.Caml in
  let f = Taglib.File.open_file `Autodetect fname in
  let prop = Taglib.File.properties f in
  let tags = Core.Std.Hashtbl.Poly.create () ~size:(Hashtbl.length prop) in
  Taglib.File.close_file f;
  Hashtbl.fold
    (fun k v seed ->
     let data = (String.concat "::" v) in
     Core.Std.Hashtbl.set seed ~key:(tag_of_string k) ~data;
     seed
    ) prop tags


let print_tags label tags =
  if (Hashtbl.length tags)>0 then
    printf "%s:\n" label;
  Hashtbl.iter
    ~f:(fun ~key ~data ->
        printf "- %s : %s\n" (string_of_tag key) data
       )
    tags


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
 *)
let file_name_guesses =
  List.map [
      [Rest;Disc;Track],"(.*)[dD][\\s\\._-]*(\\d)[\\s\\.\\)tT_-]+(\\d+)[\\s\\.-]*";
      [Rest;Disc;Track;Title],"(.*)[dD][\\s\\._-]*(\\d)[\\s\\.\\)tT_-]+(\\d+)[\\s\\.-]*(.+)";
      [Disc;Track;Title],"[dD][\\s\\._-]*(\\d+)[\\s\\.\\)_-]*[tT][\\s\\.-]*(\\d+)[\\s\\.-]*(.*)";
      [Disc;Track;Title],"(\\d)[\\s\\.\\)_-]+(\\d+)[\\s\\.-]+(.*)";
      [Rest;Disctrack],"(.+?)(\\d{1,3})";
      [Rest;Disctrack;Title],"(.+)[\\s\\.-]*(\\d{3})[\\s\\.\\)_-]+(.*)";
      [Disctrack;Title],"[\\s\\.-]*(\\d{1,3})[\\s\\.\\)_-]*(.+)";
      [Track],"(.+)";
    ] ~f:(fun (expr, re) -> ( expr,Regex.create_exn ("^"^re^"$") ))

let guess_fields_from_file_name str =
  List.fold file_name_guesses ~init:[]
            ~f:(fun seed (expr, re) ->
                match Regex.find_submatches re str with
                | Ok m -> (expr,Array.filteri ~f:(fun i _ -> i>0) m) :: seed
                | Error _ -> seed
               ) |> List.rev

let extract_disctrack dt =
  let sub s pos len = (String.sub ~pos:pos ~len:len s) in
  (* these are usually dnn where d is the disc number and nn the track number, if not
  it'll be interpreted as a track number*)
  match (String.length dt) with
  | 3 -> (Some (sub dt 0 1),Some (sub dt 1 2))
  | _ -> (None, Some dt)

(* try to extract dates from a string, and return it in YY-MM-DD format. User input might be required.
*)
let date_delims = "\\s\\.\\_/:-"
let guess_date_pat_1 = Regex.create_exn (sprintf "(\\d{1,4})[%s]+(\\d{1,4})[%s]+(\\d{1,4})" date_delims date_delims)

let guess_date_1 str =
  let sub s pos len = (String.sub ~pos:pos ~len:len s) in
  let matches r s = Regex.matches (Regex.create_exn r) s in
  let guess_mm_dd u v =
    if (int_of_string v)>12 then
      (u,v)
    else
      (v,u) in
  let guess_yy t u v =
    if (int_of_string v)>31 then
      (v,t,u)
    else if (int_of_string u)>31 then
      (u,t,v)
    else
      (t,u,v) in

  let return y d2 d3 =
    let m,d = guess_mm_dd d2 d3 in
    Some (sprintf "%02d-%02d-%02d" (int_of_string y) (int_of_string m) (int_of_string d)) in
  let return_guess d1 d2 d3 =
    let y,u,v = guess_yy d1 d2 d3 in
    return y u v in
  match Regex.find_submatches guess_date_pat_1 str with
  | Error _ -> (match (String.length str) with (* no delims *)
       | 8 ->
          (* it can be yyyy---- or ----yyyy ,where yyyy is matched against 20yy or 19yy *)
          if matches "(20|19)\\d{6}" str then
            return (sub str 2 2) (sub str 6 2) (sub str 4 2)
          else if matches "\\d{4}(20|19)\\d{2}" str then
            return (sub str 6 2) (sub str 0 2) (sub str 2 2)
          else None
       | 6 -> return_guess (sub str 0 2) (sub str 2 2) (sub str 4 2)
       | _ -> None)

  | Ok mts ->
     match mts with
              | [| _; Some d1; Some d2; Some d3 |] ->
                 if matches "(20|19)\\d{2}" d1 then
                   return (sub d1 2 2) d3 d2
                 else if matches "(20|19)\\d{2}" d3 then
                   return (sub d3 2 2) d2 d1
                 else
                   return_guess d1 d3 d2
              | _ -> Some "foo"


let guess_date str =
  match Regex.find_first (Regex.create_exn (sprintf "(\\d[%s]*)+" date_delims)) str with
  | Ok s ->  (match guess_date_1 s with
             | Some x -> x
             | None -> str)
  | Error _ -> str


let guess_fields str =
  let file_name_guesses = Hashtbl.Poly.create () ~size:6 in
  let set k d = Hashtbl.set file_name_guesses ~key:k ~data:d in
  let guesses = guess_fields_from_file_name str in
  (match guesses with
  | (expr,matches) :: _ ->
     List.iter2_exn expr (Array.to_list matches)
                    ~f:(fun t m ->
                        match m with
                        | Some mm -> (match t with
                                      | Disctrack ->
                                         (match extract_disctrack mm with
                                          | (Some disc,Some track) ->
                                             set Disc disc;
                                             set Track track
                                          | (None,Some track) ->
                                             set Track track
                                          | (_,_) -> ()
                                         )
                                      | _ -> set t mm
                                     )
                        | None -> ();
                       );
  | _ -> ()
  );
  set Date (guess_date str);
  file_name_guesses

let string_of_guess_fields fields =
  Hashtbl.fold ~init:[] ~f:(fun ~key ~data seed ->
                           (sprintf "%s: %s" (string_of_tag key) data)::seed) fields |>
    String.concat ~sep:","

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
         fun (s) -> guess_fields s |> string_of_guess_fields
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

let show =
  Command.basic
    ~summary: "Show file tags"
    Command.Spec.(empty
                  +> flag "-a" (optional string) ~doc:"artist Artist"
                  +> flag "-l" (optional string) ~doc:"location Location (will be prefixed by date if found)"
                  +> flag "-b" (optional string) ~doc:"album Album name"
                  +> flag "-t" (optional string) ~doc:"title Title name"
                  +> flag "-n" (optional int) ~doc:"track Track number"
                  +> flag "-d" (optional int) ~doc:"disc Disc number"
                  +> flag "-e" no_arg ~doc:"erase Erase all non specified tags"
		              +> anon (maybe ("file" %: readable_file))
  )
		(fun ar lo al ti tr di erase file () ->
     let cmd_line_tags = Hashtbl.Poly.create () ~size:6 in
     let string_of_int_opt = function
       | Some t -> Some (string_of_int t)
       | None -> None in
     let set_tag = function
       | (t,Some vv) -> Hashtbl.set cmd_line_tags ~key:t ~data:vv
       | (_,None) -> () in
     List.iter ~f:set_tag
               [Artist,ar;
                Location,lo;
                Album,al;
                Title,ti;
                Track,string_of_int_opt tr;
                Disc,string_of_int_opt di;
               ];

     match file with
     | Some x ->
        let filename_fields = guess_fields (Filename.basename x) in
        let dirname_fields = guess_fields (Filename.dirname x |> Filename.basename) in
        List.iter ~f:(fun (label,tags) -> print_tags label tags)
                  ["file",single_file_tags x;
                   "basename",filename_fields;
                   "dirname",dirname_fields;
                  ]
     | None -> ()
    )

let () =
  Command.run ~version:"0.1"
	            (Command.group
                 ~summary: "tagging toolkit"
			           ["guess",guess;
			            "show",show;
                 ]
		          )
