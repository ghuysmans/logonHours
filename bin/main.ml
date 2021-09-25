open LogonHours

let dump lang l =
  print_endline "    0123456789AB0123456789AB";
  Day.european |> List.iter (fun d ->
    print_string (lang d);
    print_char ' ';
    for h = 0 to 23 do
      print_char (if l.(Day.to_int d * 24 + h) then '1' else '0')
    done;
    print_newline ()
  )

open Cmdliner

let inp =
  let doc = "raw input file" in
  Arg.(value & pos 0 (some file) None & info ~doc ~docv:"INPUT" [])

let bias =
  let doc = "bias, in hours" in
  Arg.(required & opt (some int) None & info ~doc ["b"; "bias"])

type lang =
  | English
  | French

let lang =
  let doc = "output language" in
  let c = Arg.enum ["en", English; "fr", French] in
  Arg.(value & opt c English & info ~doc ["lang"; "language"])

let clear =
  let doc = "deny logon during the specified intervals" in
  Arg.(value & flag & info ~doc ["d"; "deny"])

type output =
  | Escaped
  | Raw
  | Table

let output =
  let doc = "output format" in
  let c = Arg.enum [
    "escaped", Escaped;
    "table", Table;
    "raw", Raw
  ] in
  Arg.(value & opt c Escaped & info ~doc ["f"; "format"])

type interval = {
  day: Day.t option;
  from: int * int;
  until: int * int;
}

let time_of_string s = Scanf.sscanf s "%d:%d" (fun h m -> h, m)

let (let+) x f =
  match x with
  | None -> None
  | Some x -> f x

let intervals =
  let doc = "add interval: (day,)?hh:mm-hh:mm" in
  let parser = Arg.parser_of_kind_of_string ~kind:"interval" (fun x ->
    let+ day, range =
      match String.split_on_char ',' x with
      | [day; range] -> Some (Some (Day.of_string day), range)
      | [range] -> Some (None, range)
      | _ -> None
    in
    let+ from, until =
      match String.split_on_char '-' range with
      | [from; until] -> Some (time_of_string from, time_of_string until)
      | _ -> None
    in
    Some {day; from; until}
  ) in
  let c = Arg.conv (parser, fun _ _ -> () (* FIXME? *)) in
  Arg.(value & opt_all c [] & info ~doc ["i"; "interval"])

let main inp bias lang clear output intervals =
  let lang =
    match lang with
    | English -> Day.to_string
    | French -> Day.to_french
  in
  let inp =
    match inp with
    | None -> stdin
    | Some f -> open_in f
  in
  let w = of_string ~bias (really_input_string inp 21) in
  intervals |> List.iter (fun {day; from; until} ->
    let days =
      match day with
      | None -> Day.american
      | Some d -> [d]
    in
    days |> List.iter (fun d ->
      interval from until |> List.iter (fun h ->
        if clear then
          w#clear d h
        else
          w#set d h
      )
    )
  );
  match output with
  | Escaped -> print_endline w#to_escaped
  | Raw -> print_string w#to_string
  | Table -> dump lang w#to_local


let () =
  Term.(exit @@ eval @@
    let doc = "logonHours AD attribute manipulation tool" in
    const main $ inp $ bias $ lang $ clear $ output $ intervals,
    info "logonHours" ~doc
  )
