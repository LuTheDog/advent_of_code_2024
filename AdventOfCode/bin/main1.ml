let file_name = "input.txt"

let rec strings_from_channel channel strings = 
try
    let line = input_line channel in
    strings_from_channel channel (line :: strings)
with End_of_file -> 
    close_in_noerr channel;
    print_endline "Closing file";
    strings

let rec _print_strings_from_list list idx =
    if idx < List.length list then (
        print_endline (List.nth list idx);
        _print_strings_from_list list (idx + 1)
    )

let rec _ints_from_channel channel list1 list2 = 
    try
    let numbers = Scanf.scanf channel "%d %d" (fun n1 n2 -> [n1; n2]) in
    _ints_from_channel channel ((List.nth numbers 0) :: list1) ((List.nth numbers 1) :: list2)
    with End_of_file ->
        list1 :: list2 :: []

let ints_from_string string = 
  let channel = Scanf.Scanning.from_string string in
  Scanf.bscanf channel "%d %d" (fun n1 n2 -> [n1; n2])

let rec lists_from_string_list strings idx a b =
    if idx < (List.length strings) then (
        let nums = ints_from_string (List.nth strings idx) in
        lists_from_string_list strings (idx + 1) ((List.nth nums 0) :: a) ((List.nth nums 1) :: b)
    )
    else
    [a; b]

let rec calc_el_diff_lists a b idx res =
    if idx < (List.length a) then (
        let new_res = res + (Int.abs ((List.nth a idx) - (List.nth b idx))) in
        calc_el_diff_lists a b (idx + 1) new_res
    )
    else
        res

let rec populate_hash_table list idx =
    if idx < (List.length list) then (
        let table = populate_hash_table list (idx + 1) in
        let curr = (List.nth list idx) in
        let curr_val = (Hashtbl.find_opt table curr) in
        match curr_val with
        | None -> Hashtbl.add table curr 1; table
        | Some v -> Hashtbl.replace table curr (v + 1); table
    ) else (
        Hashtbl.create ((List.length list) / 4);
    )

let rec calculate_similarity_score list idx table =
    if idx < (List.length list) then (
        let score = calculate_similarity_score list (idx + 1) table in
        let curr = List.nth list idx in
        let curr_val = Hashtbl.find_opt table curr in
        match curr_val with
        | None -> score
        | Some v -> (score + v * curr)
    ) else
        0

let () = 
    let channel = open_in file_name in
    let file_strings = strings_from_channel channel [] in
    let lists = lists_from_string_list file_strings 0 [] [] in
    let sorted_lists = [(List.stable_sort compare (List.nth lists 0)); (List.stable_sort compare (List.nth lists 1))] in
    print_endline "The differences between files are:";
    print_endline (Int.to_string (calc_el_diff_lists (List.nth sorted_lists 0) (List.nth sorted_lists 1) 0 0));
    let hashTable = populate_hash_table (List.nth lists 1) 0 in
    let similarity_score = calculate_similarity_score (List.nth lists 0) 0 hashTable in
    print_endline "Similarity score is:";
    print_endline (Int.to_string similarity_score);
    



