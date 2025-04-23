let print_grid_with_coords grid =
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      Printf.printf "(%d,%d):%c " i j grid.(i).(j);
    done;
    print_newline ()
  done

let get_next_char c =
  match c with
  | 'X' -> 'M'
  | 'M' -> 'A'
  | 'A' -> 'S'
  | _ -> raise (Invalid_argument "cant put this here")

let solve lines = 
  let grid = 
    Array.init (List.length lines) (fun i ->
      Array.init 
      (String.length (List.nth lines 0))
      (fun j -> String.get (List.nth lines i) j)) in
  print_grid_with_coords grid;
  (1, 2)