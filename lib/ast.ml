type interval = {
  day: Day.t option;
  from: int * int;
  until: int * int;
}

type command =
  | Allow of interval
  | Deny of interval

type script = command list
