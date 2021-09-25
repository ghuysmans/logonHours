open Raw

let to_local raw bias =
  let local = Array.make (7 * 3 * 8) false in
  let l = ref bias in
  let i = ref 0 in
  for _day = 0 to 6 do
    for _eight_hours = 0 to 2 do
      let m = ref 1 in
      while !m <= 128 do
        local.(!l) <- raw.(!i) land !m <> 0;
        incr l; if !l >= 7 * 24 then l := !l - 7 * 24; (* FIXME? *)
        m := !m lsl 1
      done;
      incr i; (* next input byte *)
    done
  done;
  local

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
  let raw = [|
    0; 0; 0;
    0b10000000; 0b11111111; 0b00000000;
    0b10000000; 0b11111111; 0b00000000;
    0b10000000; 0b00001111; 0b00000000;
    0b10000000; 0b11111111; 0b00000000;
    0b10000000; 0b11111111; 0b00000000;
    0; 0; 0;
  |] in
  let bias = 1 in
  let l = to_local raw bias in
  dump l;
  let d = Day.of_string Sys.argv.(1) in
  let h = int_of_string Sys.argv.(2) in
  if (new wrapper bias raw)#get d h then
    print_endline "allowed"
  else
    print_endline "denied"
