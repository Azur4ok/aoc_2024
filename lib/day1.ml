let input_file = "day1.txt"

let substrct a b = abs(a - b)

let sort_list lst = List.sort compare lst

let rec print_list lst =
    match lst with
    | [] -> print_endline ""
    | hd :: tl -> print_endline hd; print_list tl

let exrtract_values lst = 
    match lst with
    | [] -> (0, 0)
    | [a; b] -> (a, b)
    | _ -> (0, 0)

let calculate_sum_distance lst1 lst2 =
    List.fold_left2 (fun acc a b -> acc + substrct a b) 0 lst1 lst2


let calculate_amount_appearance lst target = List.length (List.filter (fun x -> x = target) lst)


let calculate_similarity_score lst1 lst2 = List.fold_left (fun acc a -> acc + (a * (calculate_amount_appearance lst2 a))) 0 lst1


let rec split_in_two_list lst func = 
    match lst with
    | [] -> ([], [])
    | hd :: tl -> 
        let (left, right) = split_in_two_list tl func in
        let converted_lst = func hd in
        let (num1, num2) = exrtract_values converted_lst in
        (num1 :: left, num2 :: right)

let convert_line line = 
    let split_result = String.split_on_char ' ' line in        
    let filtered = List.filter (fun s -> String.length (String.trim s) > 0) split_result in        
    List.map int_of_string filtered


let solve_part1 lines =
    let (lst1, lst2) = split_in_two_list lines convert_line in
    let sorted_lst1 = sort_list lst1 in
    let sorted_lst2 = sort_list lst2 in
    let total_distance = calculate_sum_distance sorted_lst1 sorted_lst2 in
    total_distance


let solve_part2 lines =
    let (lst1, lst2) = split_in_two_list lines convert_line in
    let total_similarity_score = calculate_similarity_score lst1 lst2 in
    total_similarity_score

let solve lines =
    let part1 = solve_part1 lines in
    let part2 = solve_part2 lines in
    (part1, part2)