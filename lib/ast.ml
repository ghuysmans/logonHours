type interval = {
  day: Day.t option;
  from: int * int;
  until: int * int;
}

type command = bool * interval

type script = command list
