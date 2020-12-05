let replace x y s = Seq.map (fun v -> if v == x then y else v) s;;

let binary_section l r s =
    let bin_literal = s |> String.to_seq
                        |> replace l '0'
                        |> replace r '1'
                        |> List.of_seq
                        |> (fun s -> '0' :: 'b' :: s)
                        |> List.to_seq
                        |> String.of_seq in
    int_of_string bin_literal;;

let seat_id raw =
    let row = binary_section 'F' 'B' (String.sub raw 0 7) in
    let col = binary_section 'L' 'R' (String.sub raw 7 3) in
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
    let seat_ids = read_lines "resources/input.txt" |> List.map seat_id
                                                    |> List.sort compare in
    let part1 = List.fold_left max 0 seat_ids in
    let my_seat_ids = seat_ids |> List.mapi (fun i x -> if i + 1 < List.length seat_ids && x + 2 == List.nth seat_ids (i + 1) then [x + 1] else [])
                               |> List.concat in
    let part2 = List.hd my_seat_ids in
    Printf.printf "Part 1: %d\n" part1;
    Printf.printf "Part 2: %d\n" part2;
    
