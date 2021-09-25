type t = {
  index: int;
  mask: int;
}

let locate ~bias d h =
  let d = Day.to_int d in
  let d, h =
    if h < bias then
      (d + 6) mod 7, h + 24 - bias
    else
      d, h - bias
  in
  {index = d * 3 + h / 8; mask = 1 lsl (h land 7)}

let get ~bias raw d h =
  let {index; mask} = locate ~bias d h in
  raw.(index) land mask <> 0

let set ~bias raw d h =
  let {index; mask} = locate ~bias d h in
  raw.(index) <- raw.(index) lor mask

let clear ~bias raw d h =
  let {index; mask} = locate ~bias d h in
  raw.(index) <- raw.(index) land (lnot mask)

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
  let d = Day.of_string Sys.argv.(1) in
  let h = int_of_string Sys.argv.(2) in
  if get ~bias raw d h then
    print_endline "allowed"
  else
    print_endline "denied"
