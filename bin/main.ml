open LogonHours

let dump l =
  for d = 0 to 6 do
    print_int d;
    print_char ' ';
    for h = 0 to 23 do
      print_char (if l.(d * 24 + h) then '1' else '0')
    done;
    print_newline ()
  done


let () =
  let bias = 1 in
  let w = new wrapper bias [|
    0; 0; 0;
    0b10000000; 0b11111111; 0b00000000;
    0b10000000; 0b11111111; 0b00000000;
    0b10000000; 0b00001111; 0b00000000;
    0b10000000; 0b11111111; 0b00000000;
    0b10000000; 0b11111111; 0b00000000;
    0; 0; 0;
  |] in
  let l = w#to_local in
  dump l;
  let d = Day.of_string Sys.argv.(1) in
  let h = int_of_string Sys.argv.(2) in
  if w#get d h then
    print_endline "allowed"
  else
    print_endline "denied"
