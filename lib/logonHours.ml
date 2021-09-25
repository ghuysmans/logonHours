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

class wrapper bias raw = object(self)
  method array = raw

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
