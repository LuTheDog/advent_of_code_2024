let file_name = "input2.txt"
let rec strings_from_channel channel = 
try
    let line = input_line channel in
    let prev_lines = strings_from_channel channel in
    line :: prev_lines
with End_of_file -> 
    close_in_noerr channel;
    print_endline "Closing file";
    []

let rec string_to_int_array int_list idx =
  if idx < (List.length int_list) then (
    let prev_ints = string_to_int_array int_list (idx + 1) in
    (int_of_string (List.nth int_list idx)) :: prev_ints
  ) else
    []

let rec _print_ints_from_list list idx =
    if idx < List.length list then (
        print_string (Int.to_string (List.nth list idx));
        print_string " ";
        _print_ints_from_list list (idx + 1)
    )
  else 
    print_endline ""

let rec safe_recipe recipe idx increasing =
  if (List.length recipe) <= 1 then
    1
  else (
    if idx < (List.length recipe - 1) then (
      let diff = (List.nth recipe idx) - (List.nth recipe (idx + 1)) in
      if Int.abs diff > 3 then
        0
      else 
        if diff = 0 then (
          0
        ) else (
          if increasing = 0 then (
            safe_recipe recipe (idx + 1) diff
          ) else (
            if (increasing < 0 && diff > 0) || (increasing > 0 && diff < 0) then 0
            else safe_recipe recipe (idx + 1) increasing
          )
        )
    ) else (
      1
    )
  )


let rec remove_el_from_list list idx curr =
  if curr < List.length list then
    if idx = curr then
      remove_el_from_list list idx (curr + 1)
    else 
      (List.nth list curr) :: remove_el_from_list list idx (curr + 1)
  else 
    []

let rec test_recipe_without_one_element recipe idx =
  if (idx < List.length recipe) then (
    let curr_list = remove_el_from_list recipe idx 0 in
    print_endline "Testing list:";
    _print_ints_from_list curr_list 0;
    Int.max (test_recipe_without_one_element recipe (idx + 1)) (safe_recipe curr_list 0 0)
  )
  else
    0
  

let rec loop_through_strings string_list idx =
  if idx < (List.length string_list) then (
    let num_recipes = loop_through_strings string_list (idx + 1) in
    let int_list = (String.split_on_char ' ' (List.nth string_list idx)) in
    let curr_recipe = string_to_int_array int_list 0 in
    num_recipes + (safe_recipe curr_recipe 0 0)
  )
  else
    0

let rec loop_through_strings2 string_list idx =
  if idx < (List.length string_list) then (
    let num_recipes = loop_through_strings2 string_list (idx + 1) in
    let int_list = (String.split_on_char ' ' (List.nth string_list idx)) in
    let curr_recipe = string_to_int_array int_list 0 in
    num_recipes + Int.max (safe_recipe curr_recipe 0 0) (test_recipe_without_one_element curr_recipe 0 )
  )
  else
    0

let () =
  let channel = open_in file_name in
  let file_strings = strings_from_channel channel in
  let ans = loop_through_strings file_strings 0 in
  print_endline (Int.to_string ans);
  let ans2 = loop_through_strings2 file_strings 0 in
  print_endline "Part 2:";
  print_int ans2

