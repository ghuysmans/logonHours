let hours = ['0'-'9'] | ['0'-'1']['0'-'9'] | '2'['0'-'3']
let minutes = ['0'-'5']['0'-'9']

rule tokenize = parse
| ("allow" | "a") { Parser.ALLOW }
| ("deny" | "d" | "!") { DENY }
| ("Mon" | "Lun" | "L" | "Lu") { DAY Monday }
| ("Tue" | "Mar" | "Ma") { DAY Tuesday }
| ("Wed" | "Mer" | "Me") { DAY Wednesday }
| ("Thu" | "Jeu" | "J" | "Je") { DAY Thursday }
| ("Fri" | "Ven" | "V" | "Ve" | "Vdd") { DAY Friday }
| ("Sat" | "Sam" | (* "S" | *) "Sa") { DAY Saturday }
| ("Sun" | "Dim" | "D" | "Di") { DAY Sunday }
| ',' { COMMA }
| (hours as h) (':' (minutes as m) | 'h' (minutes as m')?) {
  TIME (
    int_of_string h,
    match m, m' with
    | None, None -> 0
    | None, Some x -> int_of_string x
    | Some x, None -> int_of_string x
    | _ -> assert false (* disjoint *)
  )
}
| '-' { MINUS }
| ' '+ { tokenize lexbuf }
| '\r'? '\n' { NL }
| eof { EOF }
