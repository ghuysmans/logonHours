{
type t =
  | Any
  | English
  | French
  | Mixed

let lang = ref Any

let check l =
  match !lang, l with
  | Any, `English -> lang := English
  | Any, `French-> lang := French
  | English, `English | French, `French | Mixed, _ -> ()
  | English, `French | French, `English ->
    prerr_endline "warning: mixed languages"; (* TODO loc? *)
    lang := Mixed

let en (x : Parser.token) = check `English; x
let fr (x : Parser.token) = check `French; x
}

let hours = ['0'-'9'] | ['0'-'1']['0'-'9'] | '2'['0'-'3']
let minutes = ['0'-'5']['0'-'9']

rule tokenize = parse
| "allow"                    { en ALLOW }
| "autoriser"                { fr ALLOW }
| "a"                        { ALLOW }
| "deny" | "d"               { en DENY }
| "!"                        { DENY }
| "Mon"                      { en (DAY Monday) }
| "Lun" | "L" | "Lu"         { fr (DAY Monday) }
| "Tue"                      { en (DAY Tuesday) }
| "Mar" | "Ma"               { fr (DAY Tuesday) }
| "Wed"                      { en (DAY Wednesday) }
| "Mer" | "Me"               { fr (DAY Wednesday) }
| "Thu"                      { en (DAY Thursday) }
| "Jeu" | "J" | "Je"         { fr (DAY Thursday) }
| "Fri"                      { en (DAY Friday) }
| "Ven" | "V" | "Ve" | "Vdd" { fr (DAY Friday) }
| "Sat"                      { en (DAY Saturday) }
| "Sam" | (* "S" | *) "Sa"   { fr (DAY Saturday) }
| "Sun"                      { en (DAY Sunday) }
| "Dim" | "D" | "Di"         { fr (DAY Sunday) }
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
