
type tag =
  | Album
  | Artist
  | Track
  | Disc
  | Disctrack
  | Title
  | Rest
  | Year


val guess_field : string -> string

val guess_date : string -> string

val guess_fields_from_file_name : string -> (tag list * string option array) list

val normalize : string -> string
