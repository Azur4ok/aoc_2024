let input_file = "day2.txt"

let convert_line line = String.split_on_char ' ' line |> List.map int_of_string

let rec is_ascending lst = 
  match lst with
  | [] | [_] -> true
  | x :: y :: tl -> x < y && is_ascending (y :: tl)

  let rec is_descending lst = 
    match lst with
    | [] | [_] -> true
    | x :: y :: tl -> x > y && is_descending (y :: tl)

let rec is_adjacent_safe lst = 
  match lst with
  | [] | [_] -> true
  | x :: y :: tl -> abs(x - y) <= 3 && is_adjacent_safe (y :: tl)

let is_safe lst =
  if (is_ascending lst || is_descending lst) && is_adjacent_safe lst then
    true
    else false

let solve_part1 lines =
  let total_safe_lines = ref 0 in
  List.iter (fun s -> 
    let int_list = convert_line s in
    let is_safe_line = is_safe int_list in
    if is_safe_line then
      total_safe_lines := !total_safe_lines + 1;
    ) lines;
  !total_safe_lines

let remove_at_index lst index = List.filteri (fun i _ -> i <> index) lst

let check_by_removing lst condition =
  let len = List.length lst in
  let rec try_all_removals index =
    if index >= len then
      false
    else
      let new_list = remove_at_index lst index in
      if condition new_list then
        true
        else try_all_removals (index + 1)
      in
      try_all_removals 0

let solve_part2 lines = 
  let total_safe_lines = ref 0 in
   List.iter (fun s -> 
    let int_list = convert_line s in
    let is_safe_line = is_safe int_list in
    if is_safe_line then
      total_safe_lines := !total_safe_lines + 1
    else
      let is_safe_now = check_by_removing int_list is_safe in
      if is_safe_now then
        total_safe_lines := !total_safe_lines + 1
    ) lines;
  !total_safe_lines

let solve lines =
  let part1 = solve_part1 lines in
  let part2 = solve_part2 lines in
  (part1, part2);