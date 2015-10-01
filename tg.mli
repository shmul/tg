
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

val guess_date : string -> string option

val all_guesses : string -> (tag list * string option array) list

val normalize : string -> string
