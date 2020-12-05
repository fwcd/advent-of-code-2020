let replace s x y = Seq.map (fun v -> if v == x then y else v) s

let binary_section s l r = int_of_string (String.of_seq (List.to_seq ('0' :: 'b' :: List.of_seq (replace l r (String.to_seq s)))))

let read_lines filename =
    (* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/5775024#5775024 *)
    let lines = ref [] in
    let channel = open_in filename in
    try
        while true; do
            lines := input_line channel :: !lines
        done; !lines
    with End_of_file ->
        close_in channel;
    List.rev !lines;;

let () =
    List.iter print_endline (read_lines "resources/input.txt")
    
