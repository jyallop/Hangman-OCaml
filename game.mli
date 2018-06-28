type status 

type t = 
    | Won of { value: string; guess_count: int }
    | Lost of {current: string}
    | In_progress of { current: status list; guess_count: int; guessed_char: char list}

val new_game: string -> t

val update_game: t -> char -> t

val print_game: t -> unit