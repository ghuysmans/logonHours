module Day = Day

type t = {
  index: int;
  mask: int;
}

let locate ~bias d h =
  let t = (Day.to_int d * 24 + h - bias + 7 * 24) mod (7 * 24) in
  {index = t / 8; mask = 1 lsl (t land 7)}

let%test "p-6 first" = locate ~bias:(-6) Day.Sunday 0 = {index = 0; mask = 64}
let%test "p-6 last" = locate ~bias:(-6) Day.Saturday 23 = {index = 0; mask = 32}
let%test "p0" = locate ~bias:0 Day.Sunday 0 = {index = 0; mask = 1}
let%test "p1" = locate ~bias:1 Day.Sunday 1 = {index = 0; mask = 1}

class wrapper bias raw = object(self)
  method to_string =
    Array.map char_of_int raw |> Array.to_seq |> String.of_seq

  method to_escaped =
    Array.map (Printf.sprintf "\\%02x") raw |> Array.to_list |> String.concat ""

  method get d h =
    let {index; mask} = locate ~bias d h in
    raw.(index) land mask <> 0

  method set d h =
    let {index; mask} = locate ~bias d h in
    raw.(index) <- raw.(index) lor mask

  method clear d h =
    let {index; mask} = locate ~bias d h in
    raw.(index) <- raw.(index) land (lnot mask)

  method iter f =
    let i = ref 0 in
    for _day = 0 to 6 do
      for _eight_hours = 0 to 2 do
        let m = ref 1 in
        while !m <= 128 do
          f (raw.(!i) land !m <> 0);
          m := !m lsl 1
        done;
        incr i
      done
    done

  method to_local =
    let local = Array.make (7 * 3 * 8) false in
    let i = ref ((bias + 7 * 24) mod (7 * 24)) in
    self#iter (fun allowed ->
      local.(!i) <- allowed;
      incr i; if !i >= 7 * 24 then i := !i - 7 * 24
    );
    local
end

let of_string ~bias s =
  if String.length s = 21 then
    String.to_seq s |> Array.of_seq |> Array.map int_of_char |> new wrapper bias
  else
    failwith "LogonHours.of_string: invalid length"

let%test "of_string" =
  let w = of_string ~bias:(-6) "\x1F\x00\x1C\x00\xE0\xFF\x00\xE0\xFF\x3F\xE0\xFF\x00\x00\xF0\x03\xE0\xFF\x00\xC0\x1F" in
  w#get Day.Monday 7 && w#get Day.Monday 17 && not (w#get Day.Monday 18)

let make ~bias =
  let raw = Array.make 21 0 in
  new wrapper bias raw

let%test "set" =
  let w = make ~bias:0 in
  w#set Day.Monday 8;
  w#get Day.Monday 8

let%test "utc" =
  let w = make ~bias:1 in
  w#set Day.Monday 8; (* so that it's not all zeros *)
  w#to_string = (of_string ~bias:0 w#to_string)#to_string

let%test "l-6" = ignore (make ~bias:(-6))#to_local; true
let%test "l0" = ignore (make ~bias:0)#to_local; true
let%test "l1" = ignore (make ~bias:1)#to_local; true

let interval (h, m) (h', m') =
  if h = h' && m >= m' || (* correctness *)
     h > h' (* ensures termination *) then
    []
  else
    let upper = if m' = 0 then h' else h' + 1 in
    let rec f acc h =
      if h = upper then
        acc
      else
        f (h :: acc) (h + 1)
    in
    f [] h

let%test "i reversed" = interval (1, 0) (0, 0) = []
let%test "i empty" = interval (0, 0) (0, 0) = []
let%test "i <1" = interval (0, 0) (0, 15) = [0]
let%test "i =1" = interval (0, 0) (1, 0) = [0]
let%test "i =2" = interval (0, 0) (2, 0) = [1; 0]
let%test "i >2" = interval (0, 0) (2, 10) = [2; 1; 0]
