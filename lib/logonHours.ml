module Day = Day

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

let%test "p-6 first" = locate ~bias:(-6) Day.Sunday 0 = {index = 0; mask = 64}
let%test "p-6 last" = locate ~bias:(-6) Day.Saturday 23 = {index = 0; mask = 32}
let%test "p0" = locate ~bias:0 Day.Sunday 0 = {index = 0; mask = 1}
let%test "p1" = locate ~bias:1 Day.Sunday 1 = {index = 0; mask = 1}

class wrapper bias raw = object(self)
  method to_string =
    Array.map char_of_int raw |> Array.to_seq |> String.of_seq

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
    let i = ref bias in
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
