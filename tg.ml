open Core.Std
module Regex = Re2.Regex


type track = {
    track : int;
    album : string;
    artist : string;
    title : string;
    disc : int option;
    rest : (string, string) Hashtbl.t;
  }

(* several heuristics - d/D as prefix for disc; t/T for track; free text after numbers as
   title; 5-8 digits - date; 3 digits - disc and track
 *)
let guess_field str =
  str


(* input is assumed to be a list of file names. Try to figure out fields by diffing them
*)
let guess_fields_from_file_name lines =
  ()


(* try to extract dates from strings, and return it in YY-MM-DD format. User input might
   be required.
*)
let guess_date str =
  str

(* drop extra spaces; capitalize; decode; remove "_#*"; *)
let split_pats = Regex.create_exn "[ _%#\\*]+"

let normalize str =
  let parts = Regex.split split_pats str in
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
