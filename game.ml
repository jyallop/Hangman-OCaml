open Core_kernel

type status = {value: char; guessed: bool}

type t = 
    | Won of { value: string; guess_count: int }
    | Lost of {current: string}
    | In_progress of { current: status list; guess_count: int; guessed_char: char list}

let generate_state_list word =
    let chars = String.to_list word
    in
    List.map ~f:(fun x -> {value = x; guessed = (x = ' ')}) chars

let new_game word =
    let status_list = generate_state_list word 
    in 
    In_progress {
        current = status_list;
        guess_count = 6;
        guessed_char = [];
    }

let current_output current = 
    List.map ~f:(fun x -> match x.guessed with 
                            | false -> if x.value = ' ' then ' ' else '-'
                            | true -> x.value) current
    |> String.of_char_list

let current_to_string current =
    List.map ~f:(fun x -> x.value) current
    |> String.of_char_list

let update_game game guess = 
    match game with 
    | In_progress x -> 
        let decrement = (match List.exists ~f:(fun x -> x.value = guess) x.current with 
                            | true -> 0
                            | false -> 1)
        in
        let next = List.map ~f:(fun x -> {value = x.value; guessed = (x.value = guess || x.guessed)}) x.current in
        let guessed_all = List.for_all ~f:(fun x -> x.guessed) next in 
        (match guessed_all with 
        | true -> Won {value = (current_to_string x.current); guess_count = x.guess_count}
        | false -> if x.guess_count = 0 then Lost {current = current_to_string next}
                    else In_progress {current = next; guess_count = (x.guess_count - decrement); guessed_char = (guess::x.guessed_char)})
    | _ -> failwith "shouldn't reach here"

let characters_guessed chars =
    let string = List.map ~f:(fun x -> String.of_char x) chars
    in 
    String.concat ~sep:" " string

let print_game game =
    match game with
    | In_progress x -> let output = current_output x.current in 
                        let guessed = characters_guessed x.guessed_char
                        in 
                        Printf.fprintf stdout "%-20s -- guesses remaining: %d -- characters guessed: %-14s\n" output x.guess_count guessed
    | Won x -> Printf.fprintf stdout "You Win! The word/phrase was %s and you had %d guesses remaining\n" x.value x.guess_count
    | Lost x -> Printf.fprintf stdout "You lose, sorry the word/phrase was %s\n" x.current 