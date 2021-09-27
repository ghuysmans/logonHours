let day = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun" |
          "Lun" | "Mar" | "Mer" | "Jeu" | "Ven" | "Sam" | "Dim" as d
let hours = ['0'-'9'] | ['0'-'1']['0'-'9'] | '2'['0'-'3']
let minutes = ['0'-'5']['0'-'9']
(*
TODO reuse bindings?
let time = hours (':' minutes | 'h' minutes?)
let interval = (time as t) ('-' time as t')?
*)

rule interval = parse
| (day ','? ' '*)?
  (hours as h) (':' (minutes as m) | 'h' (minutes as m')?)
  ('-' (hours as h2) (':' (minutes as m2) | 'h' (minutes as m2')?))? {
    let i = int_of_string in
    let f m m' =
      match m, m' with
      | None, None -> 0
      | None, Some x -> i x
      | Some x, None -> i x
      | _ -> assert false (* disjoint *)
    in
    {
      Ast.day = Option.map Day.of_string d;
      from = i h, f m m';
      until =
        match h2 with
        | None -> i h + 1, f m m'
        | Some h2 -> i h2, f m2 m2'
    }
  }
and tokenize = parse
| ("allow"|"a")? ' '* { Parser.COMMAND (true, interval lexbuf) }
| ("deny"|"d"|"!") ' '* { COMMAND (false, interval lexbuf) }
| '\r'? '\n' { NL }
| eof { EOF }

{
  let l x = interval (Lexing.from_string x)
  let%test _ = l "8:00" = {day = None; from = 8,0; until = 9,0}
  let%test _ = l "Tue 8:00" = {day = Some Day.Tuesday; from = 8,0; until = 9,0}
  let%test _ = l "Tue, 8:00" = {day = Some Day.Tuesday; from = 8,0; until = 9,0}
  let%test _ = l "8:00-9:00" = {day = None; from = 8,0; until = 9,0}
}
