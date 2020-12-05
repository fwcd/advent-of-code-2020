let replace s x y = Seq.map (fun v -> if v == x then y else v) s;;

let binary_section s l r =
    let bin_literal = (String.of_seq (List.to_seq ('0' :: 'b' :: List.of_seq (replace (replace (String.to_seq s) l '0') r '1')))) in
    int_of_string bin_literal;;

let seat_id raw =
    let row = binary_section (String.sub raw 0 7) 'F' 'B' in
    let col = binary_section (String.sub raw 7 3) 'L' 'R' in
    row * 8 + col;;

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
    let seat_ids = List.sort compare (List.map seat_id (read_lines "resources/input.txt")) in
    let part1 = List.fold_left max 0 seat_ids in
    Printf.printf "Part 1: %d\n" part1;
    let my_seat_ids = List.concat (List.mapi (fun i x -> if i + 1 < List.length seat_ids && x + 2 == List.nth seat_ids (i + 1) then [x + 1] else []) seat_ids) in
    let part2 = List.hd my_seat_ids in
    Printf.printf "Part 2: %d\n" part2;
    
