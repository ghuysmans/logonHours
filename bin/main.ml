open LogonHours

let dump l =
  Day.european |> List.iter (fun d ->
    print_string (Day.to_string d);
    print_char ' ';
    for h = 0 to 23 do
      print_char (if l.(Day.to_int d * 24 + h) then '1' else '0')
    done;
    print_newline ()
  )


let () =
  let bias = 1 in
  let w = of_string ~bias (really_input_string stdin 21) in
  let l = w#to_local in
  dump l;
  let d = Day.of_string Sys.argv.(1) in
  let h = int_of_string Sys.argv.(2) in
  if w#get d h then
    print_endline "allowed"
  else
    print_endline "denied"
