
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
    | "DISC" -> Disc
    | "YEAR" -> Year
    | _ -> Rest


(* useful links http://help.mp3tag.de/main_tags.html and http://www.xiph.org/vorbis/doc/v-comment.html *)
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

let scanner_pat = Regex.create_exn ~options:([`Case_sensitive false]) "%[abdint]|[\\(\\)\\[\\]]"
let scanner_upper_pat = Regex.create_exn "%[A-Z]"

let parse_scanner_string str =
  let parts = Regex.split ~include_matches:true scanner_pat (Core.Std.Caml.String.trim str) |>
                List.map
                  ~f:(fun x ->
                      let upper = if Regex.matches scanner_upper_pat x then "?" else "" in
                      let sfrag = ".+"^upper in
                      let ifrag = "\\d+"^upper in
                      match x with
                      | "%a" | "%A" -> (Tag Artist,sfrag,true)
                      | "%b" | "%B" -> (Tag Album,sfrag,true)
                      | "%d" | "%D" -> (Tag Disc,ifrag,true)
                      | "%i" | "%I" -> (Literal,sfrag,true)
                      | "%n" | "%N" -> (Tag Track,ifrag,true)
                      | "%t" | "%T" -> (Tag Title,sfrag,true)
                      | "(" | ")" -> (Literal,"\\"^x,false)
                      | _ -> (Literal,x,false)
                     ) |> List.filter ~f:(fun (_,x,_)-> x<>"") in
  let pattern = List.map parts ~f:(fun (_,x,c) -> if c then ("("^x^")") else x) |>
                  String.concat ~sep:"" in
  (List.filter parts ~f:(fun (_,_,c) -> c) |> List.map ~f:(fun (x,_,_) -> x),
   Regex.create_exn pattern)


let scan_string field parts pattern =
  let tags = Hashtbl.Poly.create () ~size:6 in
  match Regex.find_submatches pattern field with
  | Error _ -> raise Not_found
  | Ok m ->
     try
       let scanned = Array.to_list m |> List.tl_exn |> List.filter_opt in
(*
              printf "%d %d\n%s\n%s\n" (List.length scanned) (List.length parts)
              (String.concat ~sep:"," scanned) (Regex.pattern pattern);
 *)
       List.iter2_exn
         scanned parts
         ~f:(fun sc p -> match p with
                         | Tag t -> Hashtbl.set tags ~key:t
                                                ~data:(Core.Std.Caml.String.trim sc)
                         | _ -> ()
            );
       tags
     with
       Invalid_argument _ -> raise (Failure "Scanning error")


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
    Core.Std.List.iter ["ALBUMARTIST";"GENRE";"ENCODEDBY";"COMPOSER";"COPYRIGHT"]
                       ~f:(fun t -> Hashtbl.replace prop t [""]);
    match Hashtbl.find_all prop "COMMENT" with
    | x :: _ -> if (Regex.matches useless_comment_pat (List.hd x)) then
                  Hashtbl.replace prop "COMMENT" [""]
    | _ -> ()
  );

  if not dryrun then (
    Taglib.File.set_properties f prop;
    (*
    match Hashtbl.find_all prop "TRACK" with
    | x :: _ -> printf "track: %s\n" (List.hd x)
    | _ -> ();
     *)
    ignore(Taglib.File.file_save f);
    Taglib.File.close_file f
  ) else (
    print_tags_hashtbl tags
  );
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
      [Disc],"disc*(\\d+).*";
      [Disc],"cd*(\\d+).*";
      [Rest;Disc],"(.*?)disc[\\s\\.-]*(\\d+)";
      [Rest;Disc],"(.*?)cd[\\s\\.-]*(\\d+)";
      [Disc;Track;Title],"d[\\s\\._-]*(\\d+)[\\s\\.\\)_-]*t[\\s\\.-]*(\\d+)[\\s\\._-]*(.*)";
      [Rest;Disc;Track],"(.*)d[\\s\\._-]*(\\d)[\\s\\.\\)t_-]+(\\d+).*";
      [Rest;Disc;Track;Title],"(.*)d[\\s\\._-]*(\\d)[\\s\\.\\)t_-]+(\\d+)[\\s\\._-]*(.+)";
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
let date_delims = "\\s\\._/:-"
let guess_date_pat_1 = Regex.create_exn (sprintf "(\\d{1,4})[%s]+(\\d{1,4})[%s]+(\\d{1,4})" date_delims date_delims)

let guess_date_1 raw_str =
  let str = Str.global_replace (Str.regexp (sprintf "[%s]" date_delims)) " " raw_str in
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

let location_pat = Regex.create_exn "[\\d/:\\.\\s-]+(.+)"
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
  let replace_underscore x = Str.global_replace (Str.regexp "_") " " x in
  String.concat ~sep:" " (List.filter parts ~f:(fun x -> (String.length x)>0) |>
                            List.map ~f:String.lowercase |>
                            List.map ~f:String.capitalize ) |>
    replace_underscore


(*
[Album; Artist]
Global choices are set for all;
The settings for all globals are displayed, and the user can enter an empty string to
accept the default; *N to choose from the calculated fields; or to type an alternative

[Track; Disc; Title] (* Disctrack *)
Per track fields are shown afterwards; the user can choose an empty string to accept the default; *N to choose from the calculated; '!' to accept the last selection (symbolically) for all; or to type something
 *)

type base_choice =
  | Tag
  | Empty

type choice =
  | Default of base_choice
  | Pointer of int
  | Literal of string
  | Scan of choice * scanner_parts list * Regex.t
  | Quit
  | Autonumber
  | Skip

let find_first_non_empty fields t =
  List.find_map ~f:(fun (_,tags) -> Hashtbl.find tags t)
                fields

let default_string fields t default = match default with
  | Tag -> (match find_first_non_empty fields t with
            | Some x -> x
            | None -> "not found")
  | Empty -> ""


let interpolate_pointer fields t p =
  (match List.nth fields (p-1) with
   | Some (_,tags) ->
      (match Hashtbl.find tags t with
       | Some y -> Some y
       | None -> None
      )
   | None -> None
  )

let interpolate_base_choice fields t = function
  | Default df -> default_string fields t df
  | Literal v -> v
  | Pointer c -> (match interpolate_pointer fields t c with
                  | Some v -> v
                  | _ -> raise Not_found)
  | _ -> raise (Invalid_argument "unexpected base choice")


let user_choice ?(readl=read_line) ?(printl=printf) fields t default =
  let rec read_user_choice field_indices =
    let scanner str base_choice =
      let confirm tags scan =
        print_tags_hashtbl tags;
        printl "Happy? [Y,n,q]> ";
        try
          match (String.prefix (readl ()) 1) with
          | "y" | "Y" | "" -> scan
          | "n" | "N" -> read_user_choice field_indices
          | "q" | "Q" -> Quit
          | _ -> printl "Let's try one more time, ok?\n";
                 read_user_choice field_indices;
        with Failure _ -> (
          printl "Try again";
          read_user_choice field_indices
        ) in
      let (parts,pattern) = parse_scanner_string str in
      (* TODO - returns the added input, not the field it is based on *)
      let value = interpolate_base_choice fields t base_choice in
      confirm (scan_string value parts pattern) (Scan (base_choice,parts,pattern)) in

    printl "> ";
    let line = readl () in
    let rest = String.drop_prefix line 1 in
    match (String.prefix line 1) with
    | "q" | "Q" when rest="" -> Quit
    | "s" | "S" when rest="" -> Skip
    | "!" | "a" | "A" when rest="" -> Autonumber
    | "*" ->
       let c = int_of_string (String.prefix rest 1) in
       if SI.mem field_indices c then (
         if (String.contains rest '%') then
           scanner (String.drop_prefix rest 1) (Pointer c) (* we strip the index also*)
         else
           Pointer c
       ) else (
         printf "Invalid index. Please choose again %s\n>"
                (SI.elements field_indices |> List.to_string ~f:string_of_int);
         read_user_choice field_indices
       )
    | "" -> Default default
    | _ -> if (String.contains rest '%') then
             scanner line (Default default)
           else
             (Literal (if line="-" then "" else line))
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

  match helper () with
  | Quit -> printl "Bye.\n";
            exit 0;
  | c -> (t,c)

let interpolate_choice fields idx (t,c) =
  match c with
  | Default bf -> (match bf with
                   | Empty -> None
                   | _ -> Some [t,default_string fields t bf |> normalize]
                  )
  | Pointer p -> (match interpolate_pointer fields t p with
                    | Some v -> Some [t,normalize v]
                    | None -> None)
  | Literal l -> Some [t,l]
  | Autonumber -> Some [t,string_of_int idx]
  | Scan (bc,parts,pattern) ->
     let value = interpolate_base_choice fields t bc in
     Some (Hashtbl.fold (scan_string value parts pattern)
                        ~init:[] ~f:(fun ~key ~data seed -> (key,normalize data)::seed) )
  | Skip | _ -> None

let global_choice fields =
  printf "Skip: sS | Autonumber: Aa! | Quit: Qq | *N: choose from the numbered options\n";
  let artist = user_choice fields Artist Tag in
  let date = match find_first_non_empty fields Date with
    | Some _ -> interpolate_choice fields 0 (user_choice fields Date Tag)
    | None -> None in
  let album = match date with
    | Some ((Date,d)::_) ->
       (match (interpolate_choice fields 0 (user_choice fields Location Empty)) with
        | Some [(Location,l)] -> (Album,Literal (d^" - "^l))
        | _ -> user_choice fields Album Tag
       )
    | _ -> user_choice fields Album Tag in
  [artist;album]


let pertrack_choice fields =
  List.map ~f:(fun t -> user_choice fields t Tag)
           [Title;Track;Disc]


let apply_choices idx fields choices =
  List.fold
    choices
    ~init:(Hashtbl.Poly.create () ~size:6)
    ~f:(fun seed (t,c) ->
        (match interpolate_choice fields idx (t,c) with
         | Some cc -> List.iter cc ~f:(fun (t,ch) -> Hashtbl.set seed ~key:t ~data:ch)
         | None -> ()
        );
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
         if (Sys.is_directory pathname)=`Yes && filename<>"__MACOSX" then
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

let token_pat = Str.regexp "[ \t]+"
let line_skip_pats =
  List.map [
      "show";
      "set";
      "encore";
      "e:";
      "cd";
      "disc";
      "artwork";
      "kbps";
    ] ~f:(fun re -> Regex.create_exn ~options:([`Case_sensitive false]) re)

let prefix_pat = Regex.create_exn "^[\\d_\\.\\-\\:\\(\\)\\[\\]]+\\s+"
let suffix_pat = Regex.create_exn "\\s+[\\d\\(\\[][\\d_\\.\\-:\\)\\]\\s]+$"
let scrub_pat = Regex.create_exn "\\s+[\\*\\^@\\+]+[\\d]*$"

let num_tokens line =
  Str.split token_pat line |> List.length

let has_skipped_patterns line =
  List.exists line_skip_pats ~f:(fun re -> Regex.matches re line)

let line_prefix_suffix line =
  (* printf "%s\n" (String.concat ~sep:";" skip_prefix); *)
  let clean = Regex.split prefix_pat line |> String.concat ~sep:"" |>
                Regex.split suffix_pat |> String.concat ~sep:"" |>
                Regex.split scrub_pat |> String.concat ~sep:"" in
  Regex.matches prefix_pat line,Regex.matches suffix_pat line,clean

type stamp = { raw_line_num: int; kept_line_num: int; num_tokens: int; prefix: bool; suffix: bool; clean: string; raw: string }

(* each lines gets a set of attributes. Lines with 'set', 'encore', 'show' and 'cd' are skipped but only if they have <= 2 tokens
- original line number
- line number after filtering empty and skipped lines
- number of tokens per line
- prefix (track number)
- suffix (track length)
 *)
let stamp_text_lines lines =
  let stamp idx (kept_num,stamped) line =
    let nt = num_tokens line in
    let skip = has_skipped_patterns line in
    if skip && nt<=3 then
      (kept_num,stamped)
    else (
      let prefix,suffix,clean = line_prefix_suffix line in
      (*printf "%i %i %i %B %B %s\n" idx (kept_num+1) nt prefix suffix clean;*)
      if String.is_empty clean then
        (kept_num,stamped)
      else
        (kept_num+1,
         {raw_line_num = idx;
          kept_line_num = kept_num+1;
          num_tokens = nt; prefix = prefix; suffix = suffix;
          clean = clean; raw = line} :: stamped)
    )
  in
  let _,stamped = List.foldi ~init:(0,[]) ~f:stamp lines in
  List.rev stamped

(* a greedy algorithm - we scan the file from the first (filtered) line and look for lines of the same format (prefix, suffix)
  giving lower scores to solutions that contain long lines (the aggregated number of tokens is a penalty). We allow some tolerance
 to consecutive lines that don't have the same prefix/suffix to accomodate lines that begin with a number.
  preferance is given to results that contain a prefix or a suffix and are later in the file
 *)
let guess_track_names file_lines num_tracks =
  let allowed_misses = 3 in
  let lines = Array.of_list (stamp_text_lines file_lines) in
  let index_score i =
    (*printf "%i ----------\n" i;*)
    let rec helper j tokens_count prefix suffix misses =
      if j<i+num_tracks then (
        let p = lines.(j).prefix in
        let s = lines.(j).suffix in
        let num_tokens = tokens_count+lines.(j).num_tokens in
        (*printf "%i %i %i %i %s\n" i j misses tokens_count lines.(j).clean;*)
        if p=prefix && s=suffix then
          helper (j+1) num_tokens prefix suffix misses
        else if misses<allowed_misses then
          helper (j+1) num_tokens prefix suffix (misses+1)
        else
          (i,allowed_misses,false,false,1000)
      ) else
        (i,misses,prefix,suffix,tokens_count)
    in
    helper i 0 lines.(i).prefix lines.(i).suffix 0 in
  let compare_scores (_,m1,p1,s1,t1) (_,m2,p2,s2,t2) =
    if m1=m2 && p1=p2 && s1=s2 && t1=t2 then 0
    else if m1<m2 then -1
    else if m1>m2 then 1
    else if p1 && not p2 then -1
    else if not p1 && p2 then 1
    else if s1 && not s2 then -1
    else if not s1 && s2 then 1
    else if t1<t2 then -1
    else 1 in

  let rec loop idx scores =
    if idx<=(Array.length lines)-num_tracks then
      loop (idx+1) ((index_score idx) :: scores)
    else
      scores in
  (*List.iter scores ~f:(fun (i,m,p,s,t) -> printf "%i %B %B %i %i %s\n" m p s t i lines.(i).clean );*)
  List.map (loop 0 [] |> List.sort ~cmp:compare_scores)
           ~f:(fun (idx,_,_,_,_) -> Array.sub lines ~pos:idx ~len:num_tracks |> Array.to_list) |>
    List.map ~f:(fun x -> List.map x ~f:(fun y -> y.clean))


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

(*
let inchan = function
  | None -> In_channel.create "/dev/null"
  | Some "-" -> In_channel.stdin
  | Some filename -> open_in filename
*)
let guess =
  Command.basic
    ~summary: "Guess fields or dates"
    Command.Spec.(empty
                  +> flag "-i" (optional string) ~doc:"value Value to process"
                  +> flag "-f" no_arg ~doc:" guess field"
                  +> flag "-d" no_arg ~doc:" guess date"
                  +> flag "-n" no_arg ~doc:"Normalize"
                  +> flag "-t" (optional int) ~doc:" expected number of tracks"
                  +> flag "-s" (optional string) ~doc:"Scan"
		              +> anon (maybe ("file" %: readable_file)))
		(fun value field date norm tracks scan file () ->
     match scan with
     | Some x -> (match value with
                  | Some v ->
                     let (parts,pattern) = parse_scanner_string x in
                     print_tags_hashtbl (scan_string v parts pattern)
                  | None -> printf "Trying to scan but no value provided"
                 )
     | _ ->
        match tracks with
        | Some n -> (match file with
                     | Some f -> let guesses = guess_track_names (lines_of f) n in
                                 (match List.hd guesses with
                                 | None -> printf "basa"
                                 | Some tracks -> printf "%s\n" (String.concat tracks ~sep:"\n"))
                     | None -> ())
        | _ -> let nop s = s in
               let func =
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
  let open Filename in
  match base_file with
  | None -> None
  | Some x ->
     List.map [x; dirname x; dirname x |> dirname] ~f:(fun dir -> dir^"/"^name) |>
       List.find ~f:exists


let index_html_pats = Regex.create_exn "\\s+ (.+) [\\d:]+ <em>"
let tracks_txt_pats = Regex.create_exn "\\s*[\\d\\.\\):\\-]*\\s*(.+)"

let filter_nth_match strings pat n =
  List.map strings
           ~f:(fun l -> match Regex.find_submatches pat l with
                        | Error _ -> None
                        | Ok mts -> mts.(n)
              ) |> List.filter_opt


let read_tracks_names tracks_file first_file =
  let magic () = match List.find_map [
                           "index.html",index_html_pats;
                           "tracks.txt",tracks_txt_pats]
                                     ~f:(fun (f,p) -> match find_file f first_file with
                                                        | Some y -> Some (y,p)
                                                        | None -> None) with
    | Some (y,p) -> filter_nth_match (lines_of y) p 1
    | None -> [] in
  match tracks_file with
  | Some x when (exists x) -> lines_of x
  | _ -> magic ()

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
     let track_names = if not print then
                         read_tracks_names tracks (List.hd all_files)
                       else
                         [] in
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
         "track names",track_names_fields;
         "original",orig_fields;
         "title",title_fields;
         "album",album_fields;
         "basename",filename_fields;
         "dirname",dirname_fields;
       ] in
     let num_track_names = List.length track_names in
     let num_files = List.length all_files in
     if num_track_names>0 && num_track_names<>num_files then
       raise (Invalid_argument
                (sprintf "Track names size mismatch: actual:%d expected: %d"
                         num_track_names num_files));
     let fields =
       match List.hd all_files with
       | Some x -> printf "%s\n" x; fields_for_file 0 x
       | _ -> [] in

     List.iteri fields ~f:(fun i (label,tags) -> print_tags i label tags);
     if not print then (
       printf "------\n";
       let choices = List.append (pertrack_choice fields)  (global_choice fields) in
       let last_tags = List.foldi
                         all_files
                         ~init:empty_tags
                         ~f:(fun i _ x ->
                             let tags = apply_choices (i+1) (fields_for_file i x) choices  in
                             save_file_tags x tags erase dryrun
                            ) in
       if not dryrun then (
         printf "------\n";
         print_tags_hashtbl last_tags
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
