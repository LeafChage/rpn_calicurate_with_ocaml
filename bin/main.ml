let maybe_read_line () =
  try Some(read_line())
  with End_of_file -> None

let rec loop acc =
  match maybe_read_line () with
  | Some(line) ->
          Printf.printf "> %d \n" (Cal.parse line); loop (line :: acc)
  | None -> List.iter print_endline acc

let () = loop []
