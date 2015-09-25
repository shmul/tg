
open Core.Std
module Regex = Re2.Regex
module SI = Set.Make(Int)

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
    | "TRACK" -> Track
    | "TRACKNUMBER" -> Track
    | "ARTIST" -> Artist
    | "DISCNUMBER" -> Disc
    | "YEAR" -> Year
    | _ -> Rest

(* useful link http://help.mp3tag.de/main_tags.html *)
(* Lowercase tags are intentionally so, to indicate they aren't standard *)
let string_of_tag = function
  | Album -> "ALBUM"
  | Artist -> "ARTIST"
  | Track -> "TRACK"
  | Disc -> "DISCNUMBER"
  | Disctrack -> "disctrack"
  | Title -> "TITLE"
  | Rest -> "rest"
  | Year -> "YEAR"
  | Location -> "location"
  | Date -> "date"

let urldecode str = try Netencoding.Url.decode str with
                      | _ -> str

let empty_tags = Hashtbl.Poly.create () ~size:1

let single_file_tags fname =
  try
    let f = Taglib.File.open_file `Autodetect fname in
    let open Core.Std.Caml in

    let prop = Taglib.File.properties f in
    let tags = Core.Std.Hashtbl.Poly.create () ~size:(Hashtbl.length prop) in
    Taglib.File.close_file f;
    Hashtbl.fold
      (fun k v seed ->
       let data = (String.concat "::" v) |> urldecode in
       Core.Std.Hashtbl.set seed ~key:(tag_of_string k) ~data;
       seed
      ) prop tags
  with
    Not_found -> empty_tags

(* we look for %x or %X in the string (capitalized means non-greedy)
the symbols are:
a -> artist
b -> album
d -> disc
i -> ignore
n -> track
t -> title
 *)
type scanner_parts =
  | Literal
  | Tag of tag

let scanner_pat = Regex.create_exn ~options:([`Case_sensitive false]) "(%[abdint])"
let scanner_upper_pat = Regex.create_exn "%[A-Z]"

let parse_scanner_string str =
  let parts = Regex.split ~include_matches:true scanner_pat str |>
                List.map
                  ~f:(fun x ->
                      let upper = if Regex.matches scanner_upper_pat x then "?" else "" in
                      let sfrag = ".+"^upper in
                      let ifrag = "\\d+"^upper in
                      match x with
                      | "%a" | "%A" -> (Tag Artist,sfrag)
                      | "%b" | "%B" -> (Tag Album,sfrag)
                      | "%d" | "%D" -> (Tag Disc,ifrag)
                      | "%i" | "%I" -> (Literal,sfrag)
                      | "%n" | "%N" -> (Tag Track,ifrag)
                      | "%t" | "%T" -> (Tag Title,sfrag)
                      | _ -> (Literal,x)
                     ) |> List.filter ~f:(fun (_,x)-> x<>"") in
  let pattern = List.map parts ~f:(fun (_,x) -> "("^x^")") |>
                  String.concat ~sep:"" in
  (List.map parts ~f:(fun (x,_) -> x),Regex.create_exn pattern)


let scan_string user_str field =
  let tags = Hashtbl.Poly.create () ~size:6 in
  let (parts,pattern) = parse_scanner_string user_str in
  match Regex.find_submatches pattern field with
  | Error _ -> printf "No matches\n";
               tags
  | Ok m ->
     try
       let scanned = Array.to_list m |> List.tl_exn |> List.filter_opt in
(*       printf "%d %d\n%s\n%s\n" (List.length scanned) (List.length parts)
              (String.concat ~sep:"," scanned) (Regex.pattern pattern);
*)
       List.iter2_exn scanned parts
                      ~f:(fun sc p -> match p with
                                      | Tag t -> Hashtbl.set tags ~key:t ~data:(Core.Std.Caml.String.trim sc)
                                      | _ -> ()
                         );
       tags
     with
       Invalid_argument _ -> printf "Scanning failed\n";
                             tags



let print_tags_hashtbl tags =
  Hashtbl.iter
    ~f:(fun ~key ~data ->
        printf "  - %s : %s\n" (string_of_tag key) data
       )
    tags

let print_tags idx label tags =
  if (Hashtbl.length tags)>0 then
    printf "%d) %-10s:\n" idx label;
  print_tags_hashtbl tags

let useless_comment_pat = Regex.create_exn "^[0-9A-Fa-f\\s]+$"

let save_file_tags fname tags erase dryrun =
  let f = Taglib.File.open_file `Autodetect fname in
  let open Core.Std.Caml in
  let prop = Taglib.File.properties f in
  Core.Std.Hashtbl.iter ~f:(fun ~key ~data ->
                            Hashtbl.replace prop (string_of_tag key) [data];
                            match key with (* VorbisComments compatibility *)
                            | Track -> Hashtbl.replace prop "TRACKNUMBER" [data]
                            | Disc -> Hashtbl.replace prop "DISC" [data]
                            | _ -> ();
                            (*printf "replacing %s with %s\n" (string_of_tag key) data*)
                           ) tags;
  if erase then (
    printf "erasing\n";
    Core.Std.List.iter ["ALBUMARTIST";"GENRE";"ENCODEDBY";"COMPOSER";"COPYRIGHT"]
                       ~f:(fun t -> Hashtbl.replace prop t [""]);
    match Hashtbl.find_all prop "COMMENT" with
    | x :: _ -> if (Regex.matches useless_comment_pat (List.hd x)) then
                     Hashtbl.replace prop "COMMENT" [""]
    | _ -> ()
  );

  if not dryrun then (
    Taglib.File.set_properties f prop;
    printf "saving: %s\n" fname;
(*
    match Hashtbl.find_all prop "TRACK" with
    | x :: _ -> printf "track: %s\n" (List.hd x)
    | _ -> ();
 *)
    ignore(Taglib.File.file_save f);
    Taglib.File.close_file f
  ) else
    print_tags_hashtbl tags



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
      [Disc],"disc*(\\d+).*";
      [Disc],"cd*(\\d+).*";
      [Rest;Disc],"(.*?)disc[\\s\\.-]*(\\d+)";
      [Rest;Disc],"(.*?)cd[\\s\\.-]*(\\d+)";
      [Rest;Disc;Track],"(.*)d[\\s\\._-]*(\\d)[\\s\\.\\)t_-]+(\\d+).*";
      [Rest;Disc;Track;Title],"(.*)d[\\s\\._-]*(\\d)[\\s\\.\\)t_-]+(\\d+)[\\s\\.-]*(.+)";
      [Disc;Track;Title],"d[\\s\\._-]*(\\d+)[\\s\\.\\)_-]*t[\\s\\.-]*(\\d+)[\\s\\.-]*(.*)";
      [Disc;Track;Title],"(\\d)[\\s\\.\\)_-]+(\\d+)[\\s\\.-]+(.*)";
      [Disctrack;Title],"[\\s\\.-]*(\\d{1,3})[\\s\\.\\)_-]*(.+)";
      [Rest;Disctrack],"(.+?)(\\d{1,3})";
      [Rest;Disctrack;Title],"(.+)[\\s\\.-]*(\\d{3})[\\s\\.\\)_-]+(.*)";
      [Track],"(.+)";
    ] ~f:(fun (expr, re) -> ( expr,Regex.create_exn ~options:([`Case_sensitive false]) ("^"^re^"$") ))

let all_guesses str =
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
    printf "'%s' '%s' '%s'\n" t u v;
    if (int_of_string v)>31 then
      (v,t,u)
    else if (int_of_string u)>31 then
      (u,t,v)
    else
      (t,u,v) in
  let trim = Core.Std.Caml.String.trim in
  let return y d2 d3 =
    let m,d = guess_mm_dd (trim d2) (trim d3) in
    Some (sprintf "%02d-%02d-%02d" (int_of_string y) (int_of_string m) (int_of_string d)) in
  let return_guess d1 d2 d3 =
    let y,u,v = guess_yy (trim d1) (trim d2) (trim d3) in
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
  | Ok s -> guess_date_1 s
  | Error _ -> None

let location_pat = Regex.create_exn "[\\d/:\\.-]+\\s+(.+)"
let guess_location str =
  match Regex.find_submatches location_pat str with
  | Error _ -> None
  | Ok mts -> mts.(1)

let guess_fields str =
  let str_guesses = Hashtbl.Poly.create () ~size:6 in
  let set k d = Hashtbl.set str_guesses ~key:k ~data:d in
  (match all_guesses str with
  | (expr,matches) :: _ ->
     List.iter2_exn
       expr (Array.to_list matches)
       ~f:(fun t m ->
           match m with
           | Some mm ->
              (match t with
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

  (match guess_date str with
  | Some d -> set Date d;
  | None -> ());

  (match guess_location str with
  | Some d -> set Location d;
  | None -> ());

  str_guesses

let string_of_guess_fields fields =
  Hashtbl.fold ~init:[] ~f:(fun ~key ~data seed ->
                           (sprintf "%s: %s" (string_of_tag key) data)::seed) fields |>
    String.concat ~sep:","

(* drop extra spaces; capitalize; decode; remove "_#*"; *)
let split_pats = Regex.create_exn "[ _%#\\*]+"

let normalize str =
  let parts = Regex.split split_pats (urldecode str) in
  String.concat ~sep:" " (List.filter parts ~f:(fun x -> (String.length x)>0) |>
                            List.map  ~f:String.lowercase |>
                            List.map ~f:String.capitalize)

(*
[Album; Artist]
Global choices are set for all;
The settings for all globals are displayed, and the user can enter an empty string to
accept the default; *N to choose from the calculated fields; or to type an alternative

[Track; Disc; Title] (* Disctrack *)
Per track fields are shown afterwards; the user can choose an empty string to accept the default; *N to choose from the calculated; '!' to accept the last selection (symbolically) for all; or to type something
 *)

type default_choice =
  | Tag
(*  | Literal of string *)
  | Empty

type choice =
  | Default of default_choice
  | Pointer of int
  | Literal of string
  | Quit
  | Autonumber

let find_first_non_empty fields t =
  List.find_map ~f:(fun (_,tags) -> Hashtbl.find tags t)
                fields

let default_string fields t default = match default with
  | Tag -> (match find_first_non_empty fields t with
            | Some x -> x
            | None -> raise Not_found)
(*  | Literal x -> x *)
  | Empty -> ""

let user_choice fields t default =
  let rec read_user_choice field_indices =
    printf "> ";
    let line = read_line () in
    let rest = String.drop_prefix line 1 in
    match (String.prefix line 1) with
    | "" -> Default default
    | "q" | "Q" -> Quit
    | "!" | "a" | "A" -> Autonumber
    | "*" ->
       let c = int_of_string rest in
       printf "%d\n" c;
       if SI.mem field_indices c then
         Pointer c
       else (
         printf "Invalid index. Please choose again %s\n"
                (SI.elements field_indices |> List.to_string ~f:string_of_int);
         read_user_choice field_indices
       )
    | _ -> Literal line
  in

  let helper () =
    let df = default_string fields t default in
    printf "%-14s:    %s\n" (string_of_tag t) df;
    let show_indices idx seed (label,tags) =
      match Hashtbl.find tags t with
      | Some x ->
           printf "  %-12s: %d) %s\n" label (idx+1) x;
           SI.add seed (idx+1)
      | None -> seed in
    let field_indices = List.foldi ~init:SI.empty ~f:show_indices fields in
    read_user_choice field_indices in
  let c = helper () in
  match c with
  | Quit -> printf "Bye.\n";
            exit 0;
  | _ -> (t,c)

let interpolate_choice fields idx (t,c) =
  match c with
  | Default df -> (match df with
                  | Empty -> None
                  | _ -> Some (default_string fields t df))
  | Pointer p ->
     (match List.nth fields (p-1) with
      | Some (_,tags) ->
         (match Hashtbl.find tags t with
          | Some y -> Some y
          | None -> Some "no such tag"
         )
      | None -> Some "pointer mismatch"
     )
  | Literal l -> Some l
  | Autonumber -> Some (string_of_int idx)
  | _ -> None

let global_choice fields =
  printf "Autonumber: Aa! | Quit: Qq\n; *N to choose from the numbered options";
  let artist = user_choice fields Artist Tag in
  let date = match find_first_non_empty fields Date with
      | Some _ -> interpolate_choice fields 0 (user_choice fields Date Tag)
      | None -> None in
  let album = match date with
  | None -> user_choice fields Album Tag
  | Some d -> match (interpolate_choice fields 0 (user_choice fields Location Empty)) with
                | Some l -> (Album,Literal (d^" - "^l))
                | None -> user_choice fields Album Tag in
  [artist;album]


let pertrack_choice fields =
  List.map ~f:(fun t -> user_choice fields t Tag)
           [Title;Track;Disc]


let apply_choices fields choices =
  List.foldi
    ~init:(Hashtbl.Poly.create () ~size:6)
    choices
    ~f:(fun idx seed (t,c) ->
        let ch = match interpolate_choice fields idx (t,c) with
          | Some cc -> cc
          | None -> "_" in
        Hashtbl.set seed ~key:t ~data:ch;
        seed
       )

(* adapted from https://ocaml.org/learn/tutorials/if_statements_loops_and_recursion.html *)
let readdir_no_ex dirh =
  try
    Some (Unix.readdir dirh)
  with
    End_of_file -> None

let rec read_directory path selector : string list =
  if (Sys.is_directory path)=`Yes then (
    let open Unix in
    let selector_pat = Regex.create_exn selector in
    let dirh = opendir path in
    let rec loop () : string list =
      let filename = readdir_no_ex dirh in
      match filename with
      | None -> []
      | Some "." -> loop ()
      | Some ".." -> loop ()
      | Some filename ->
         let pathname = path ^ "/" ^ filename in
         if (Sys.is_directory pathname)=`Yes then
           List.concat [read_directory pathname selector;loop ()]
         else if (Regex.matches selector_pat pathname) then
           (pathname :: loop ())
         else
           loop () in
    let rv = loop () in
    closedir dirh;
    rv
  ) else
    [path]


(* command line handling *)
let readable_file =
  Command.Spec.Arg_type.create
    (fun fn -> In_channel.with_file fn ~f:(fun _ -> fn))

let lines_of f =
  let ic = open_in f in
  let rec loop lines =
    try
      let l = Core.Std.Caml.String.trim (input_line ic) in
      if (String.length l)>0 then
        loop (l::lines)
      else
        loop lines
    with End_of_file ->
      close_in_noerr ic;
      lines in
  loop [] |> List.rev

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
      +> flag "-s" (optional string) ~doc:"Scan"
		  +> anon (maybe ("file" %: readable_file)))
		(fun value field date norm scan file () ->
     let nop s = s in
     match scan with
     | Some x -> (match value with
                  | Some v -> print_tags_hashtbl (scan_string x v)
                  | None -> printf "Trying to scan but no value provided"
                 )
     | _ -> let func =
              if field then
                fun (s) -> guess_fields s |> string_of_guess_fields
              else if date then
                fun (s) -> match guess_date s with
                           | Some x -> x
                           | None -> ""
              else if norm then
                normalize
              else
                nop in
            match value with
            | Some x -> printf "%s -> %s\n" x (func x)
            | None -> (match file with
                       | Some f -> printf "%s -> %s\n" f (func f)
                       | None -> ())
    )

let exists f = Sys.file_exists f=`Yes

let find_file name base_file =
  match base_file with
  | None -> None
  | Some x ->
       List.map [x; Filename.dirname x] ~f:(fun dir -> dir^"/"^name) |> List.find ~f:exists

(*

let find_file name base_file =
  match base_file with
  | None -> None
  | Some x ->
     let opt1 = x^"/"^name in
     let opt2 = (Filename.dirname x)^"/"^name in
     if exists opt1 then
       Some opt1
     else if exists opt2 then
       Some opt2
     else
       None
 *)
let index_html_pats = Regex.create_exn "\\s+ (.+) [\\d:]+ <em>"
let tracks_txt_pats = Regex.create_exn "\\s+[\\d\\.\\):\\-]*\\s*(.+?)"

let filter_nth_match strings pat n =
  List.map strings
           ~f:(fun l -> match Regex.find_submatches pat l with
                        | Error _ -> None
                        | Ok mts -> mts.(n)
              ) |> List.filter_opt
let rec find_map l ~f =
  match l with
  | hd :: tl -> (match f hd with
                   | Some m -> Some m
                   | None -> find_map tl ~f)
  | _ -> None


let read_tracks_names tracks_file first_file =
  match tracks_file with
  | None -> []
  | Some x ->
     if exists x then
       lines_of x
     else (
       match find_map ["index.html",index_html_pats;"tracks.txt",tracks_txt_pats]
                      ~f:(fun (f,p) -> match find_file f first_file with
                         | Some y -> Some (y,p)
                         | None -> None) with
       | Some (y,p) -> filter_nth_match (lines_of y) p 1
       | None -> []
     )

let normalize_file_tags tags =
  let p = Regex.create_exn "(\\d+)" in
  Hashtbl.iter
    ~f:(fun ~key ~data -> match key with
                          | Track | Disc -> (match Regex.find_first p data with
                                             | Ok s -> Hashtbl.replace tags ~key ~data:s
                                             | Error _ -> ())
                          | _ -> ())
    tags;
  tags
(*
filter_nth_match (lines_of y) index_html_pats 1
       match find_file "index.html" first_file with
       | Some y -> filter_nth_match (lines_of y) index_html_pats 1
       | None -> (match find_file "tracks.txt" first_file with
                  | Some y -> filter_nth_match (lines_of y) tracks_txt_pats 1
                  | None -> [])
     )
 *)

let set =
  Command.basic
    ~summary: "Set file(s) tags"
    Command.Spec.(empty
                  +> flag "-a" (optional string) ~doc:"artist Artist"
                  +> flag "-l" (optional string) ~doc:"location Location (will be prefixed by date if found)"
                  +> flag "-b" (optional string) ~doc:"album Album name"
                  +> flag "-t" (optional string) ~doc:"title Title name"
                  +> flag "-n" (optional int) ~doc:"track Track number"
                  +> flag "-d" (optional int) ~doc:"disc Disc number"
                  +> flag "-e" no_arg ~doc:"erase Erase all non specified tags"
                  +> flag "-r" no_arg ~doc:"dryrun Dry-run, doesn't set tags"
                  +> flag "-p" no_arg ~doc:"print Just print all the tags"
                  +> flag "-f" (optional file) ~doc:"file Track names from a file"
		              +> anon (sequence ("filename" %: readable_file))
  )
		(fun ar lo al ti tr di  erase dryrun print tracks files () ->
     let all_files = List.map files ~f:(fun x -> read_directory x "\\.(mp[34]|flac|ogg)$")
                     |> List.concat |> List.sort ~cmp:compare |>
                       List.filter ~f:(fun x-> x<>"") in
     let cmd_line_tags = Hashtbl.Poly.create () ~size:6 in
     let string_of_int_opt = function
       | Some t -> Some (string_of_int t)
       | None -> None in
     let set_tag = function
       | (t,Some vv) -> Hashtbl.set cmd_line_tags ~key:t ~data:vv
       | (_,None) -> () in
     let track_names = read_tracks_names tracks (List.hd all_files) in
     List.iter ~f:set_tag
               [Artist,ar;
                Location,lo;
                Album,al;
                Title,ti;
                Track,string_of_int_opt tr;
                Disc,string_of_int_opt di;
               ];

     let fields_for_file idx x =
       let orig_fields = normalize_file_tags (single_file_tags x) in
       let title_fields = match Hashtbl.find orig_fields Title with
         | Some t -> guess_fields t
         | None -> empty_tags in
       let album_fields = match Hashtbl.find orig_fields Album with
         | Some t -> guess_fields t
         | None -> empty_tags in
       let filename_fields = guess_fields (Filename.basename x |> Filename.chop_extension) in
       let dirname_fields = guess_fields (Filename.dirname x |> Filename.basename) in
       let track_names_fields = Hashtbl.Poly.create () in
       (match List.nth track_names idx with
         | Some x -> Hashtbl.set track_names_fields ~key:Title ~data:x
         | None -> ());
       [
         "cmd line",cmd_line_tags;
         "original",orig_fields;
         "title",title_fields;
         "album",album_fields;
         "basename",filename_fields;
         "dirname",dirname_fields;
         "track names",track_names_fields
       ] in
     if (List.length track_names)>0 && (List.length track_names)<>(List.length all_files) then
       raise (Invalid_argument "Track names size mismatch");
     let fields =
       match List.hd all_files with
       | Some x -> printf "%s\n" x; fields_for_file 0 x
       | _ -> [] in

     List.iteri fields ~f:(fun i (label,tags) -> print_tags i label tags);
     if not print then (
       printf "------\n";
       let choices = List.append (pertrack_choice fields)  (global_choice fields) in
       List.iteri all_files
                  ~f:(fun i x ->
                      let tags = apply_choices (fields_for_file i x) choices  in
                      save_file_tags x tags erase dryrun
                     )
     )
    )

let () =
  Command.run ~version:"0.1"
	            (Command.group
                 ~summary: "tagging toolkit"
			           ["guess",guess;
			            "set",set;
                 ]
		          )
