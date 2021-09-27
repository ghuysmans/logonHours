type t =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

let to_int = function
  | Sunday -> 0
  | Monday -> 1
  | Tuesday -> 2
  | Wednesday -> 3
  | Thursday -> 4
  | Friday -> 5
  | Saturday -> 6

(* FIXME redundant *)
let of_string = function
  | "Mon" | "Lun" | "L" | "Lu" -> Monday
  | "Tue" | "Mar" | "Ma" -> Tuesday
  | "Wed" | "Mer" | "Me" -> Wednesday
  | "Thu" | "Jeu" | "J" | "Je" -> Thursday
  | "Fri" | "Ven" | "V" | "Ve" | "Vdd" -> Friday
  | "Sat" | "Sam" | (* "S" | *) "Sa" -> Saturday
  | "Sun" | "Dim" | "D" | "Di" -> Sunday
  | _ -> failwith "Day.of_string"

let to_string = function
  | Monday -> "Mon"
  | Tuesday -> "Tue"
  | Wednesday -> "Wed"
  | Thursday -> "Thu"
  | Friday -> "Fri"
  | Saturday -> "Sat"
  | Sunday -> "Sun"

let to_french = function
  | Monday -> "Lun"
  | Tuesday -> "Mar"
  | Wednesday -> "Mer"
  | Thursday -> "Jeu"
  | Friday -> "Ven"
  | Saturday -> "Sam"
  | Sunday -> "Dim"

let american = [Sunday; Monday; Tuesday; Wednesday; Thursday; Friday; Saturday]
let european = [Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday]
