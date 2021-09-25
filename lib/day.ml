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

let of_string = function
  | "Mon" | "Lun" | "L" | "Lu" -> Monday
  | "Tue" | "Mar" | "Ma" -> Tuesday
  | "Wed" | "Mer" | "Me" -> Wednesday
  | "Thu" | "Jeu" | "J" | "Je" -> Thursday
  | "Fri" | "Ven" | "V" | "Ve" | "Vdd" -> Friday
  | "Sat" | "Sam" | (* "S" | *) "Sa" -> Saturday
  | "Sun" | "Dim" | "D" | "Di" -> Sunday
  | _ -> failwith "Day.of_string"
