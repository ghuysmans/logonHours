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

class wrapper bias raw = object
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
end
