let print_list lst = List.iter (fun x -> Printf.printf "%d\n" x) lst

let get_all_matches line =
  let pattern = Re2.create_exn {|mul\((\d+),(\d+)\)|} in
  let number_pattern = Re2.create_exn {|(\d+),(\d+)|} in
  let matches = Re2.find_all_exn pattern line in
  let new_str = String.concat "," matches in
  let number_matches = Re2.find_all_exn number_pattern new_str in
  let nums =
    List.fold_left
      (fun acc m ->
        let num = Scanf.sscanf m "%d,%d" (fun n1 n2 -> n1 * n2) in
        [ num ] @ acc)
      [] number_matches
  in
  List.rev nums


let solve lines =
  let part1 = 
    List.fold_left 
    (fun acc line -> 
      let line_value = List.fold_left
      (fun acc num -> acc + num) 0 (get_all_matches line) in
      acc + line_value
    ) 0 lines in
  (part1, 3)
