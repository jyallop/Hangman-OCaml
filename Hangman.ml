open IO
open Random
open Game
open Core

let pick_random_word word_count =
    Random.self_init ();
    Random.int word_count
    
let () =
    let x = IO.get_word_count "words.txt" in
    let word_number = pick_random_word x in
    let word = IO.read_word word_number "words.txt" in
    let game = new_game word 
    in
    let rec game_loop game = 
        let () = print_game game in 
        let () = Out_channel.flush stdout in
        let guess = 
            match read_guess () with 
            | Some char -> char 
            | None -> failwith "Input error"
        in 
        let new_game = update_game game guess in 
        match new_game with 
        | In_progress x -> game_loop new_game
        | _ -> print_game new_game 
    in
    game_loop game