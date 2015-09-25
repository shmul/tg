
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

val scan_string : string -> string ->  (tag, string) Core_kernel.Core_hashtbl.t
