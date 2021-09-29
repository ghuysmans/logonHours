type interval = {
  day: LogonHours.Day.t option;
  from: int * int;
  until: int * int;
}

let weekday {Glical.Datetime.year; month; day; _} =
  let open Unix in
  let tm = {
    tm_sec = 0; tm_min = 0; tm_hour = 0;
    tm_mday = day; tm_mon = month - 1; tm_year = year - 1900;
    tm_wday = 0; tm_yday = 0; tm_isdst = false;
  } in
  match (snd @@ mktime tm).tm_wday with
  | 0 -> LogonHours.Day.Sunday
  | 1 -> Monday
  | 2 -> Tuesday
  | 3 -> Wednesday
  | 4 -> Thursday
  | 5 -> Friday
  | 6 -> Saturday
  | _ -> failwith "getDay"

let interval_of_event {Smartschool.Ical.event = {dtstart; dtend; _}; _} =
  let w = weekday dtstart in
  if w = weekday dtend then {
    day = Some w;
    from = dtstart.hours, dtstart.minutes;
    until = dtend.hours, dtend.minutes
  }
  else
    failwith "a course can't span two days"
