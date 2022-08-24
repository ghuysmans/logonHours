open LogonHours
open Cmdliner

let inp =
  let doc = "raw input file" in
  Arg.(value & pos 0 (some file) None & info ~doc ~docv:"INPUT" [])

let bias =
  let doc = "bias, in hours" in
  Arg.(required & opt (some int) None & info ~doc ["b"; "bias"])

let lang =
  let doc = "output language" in
  let c = Arg.enum ["en", English; "fr", French] in
  Arg.(value & opt c English & info ~doc ["lang"; "language"])

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

let (let+) x f =
  match x with
  | None -> None
  | Some x -> f x

let commands =
  let doc = "update interval: [allow|deny] [day,]hh:mm-hh:mm" in
  let parser = Arg.parser_of_kind_of_string ~kind:"interval" (fun x ->
    Some (Parser.command Lexer.tokenize (Lexing.from_string x))
  ) in
  let c = Arg.conv (parser, fun _ _ -> () (* FIXME? *)) in
  Arg.(value & opt_all c [] & info ~doc ["i"; "interval"; "e"; "execute"])

let main inp bias lang output commands =
  let w =
    match inp with
    | None -> make ~bias
    | Some f -> of_string ~bias (really_input_string (open_in f) 21)
  in
  let update allow Ast.{day; from; until} =
    let days =
      match day with
      | None -> Day.american
      | Some d -> [d]
    in
    days |> List.iter (fun d ->
      interval from until |> List.iter (fun h ->
        if allow then
          w#set d h
        else
          w#clear d h
      )
    )
  in
  commands |> List.iter (function
    | Ast.Allow i -> update true i
    | Deny i -> update false i
  );
  match output with
  | Escaped -> print_endline w#to_escaped
  | Raw -> print_string w#to_string
  | Table -> dump lang w


let () =
  Term.(exit @@ eval @@
    let doc = "logonHours AD attribute manipulation tool" in
    const main $ inp $ bias $ lang $ output $ commands,
    info "logonHours" ~doc
  )
