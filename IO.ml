open Core

let rec read_guess () =
    Out_channel.output_string stdout "Enter a guess: ";
    Out_channel.flush stdout;
    match In_channel.input_char In_channel.stdin with
    | None -> Out_channel.output_string stdout "invalid guess"; 
                Out_channel.flush stdout;
                read_guess ()
    | Some '\n' -> read_guess () 
    | Some x -> Some x

let read_word line file =
    let file_channel = In_channel.create file in
    let rec seek_line n =
        let line = 
        match In_channel.input_line file_channel with 
        | Some s -> s 
        | None -> failwith "Error reading file\n" 
        in
        match n with
        | 0 -> In_channel.close file_channel; line 
        | x -> seek_line (x - 1)
    in 
    seek_line line

let get_word_count file =
    let file_channel = In_channel.create file in
    let num_words = In_channel.fold_lines ~init:0 ~f:(fun x str -> x + 1) file_channel
    in 
    In_channel.close file_channel;
    num_words