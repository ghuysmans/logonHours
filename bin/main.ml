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
  let c = Arg.enum Demo.["en", English; "fr", French] in
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

let import =
  let doc = "import intervals from an iCal file" in
  Arg.(value & opt (some file) None & info ~doc ["import"])

let class_ =
  let doc = "target Smartschool class" in
  (* TODO opt_all? *)
  Arg.(value & opt (some string) None & info ~doc ["class"])

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

let main inp bias lang output import class_ commands =
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
  let intervals =
    match import with
    | None -> []
    | Some f ->
      let open Smartschool in
      Glical.file_contents f |>
      Ical.parse |>
      begin match class_ with
      | None -> fun l -> l
      | Some c -> List.filter (fun {Ical.class_; _} -> class_ = c)
      end |>
      List.map Date.interval_of_event
  in
  intervals |> List.iter (fun {Date.day; from; until} ->
    day |> Option.iter (fun d ->
      interval from until |> List.iter (fun h -> w#set d h)
    )
  );
  commands |> List.iter (function
    | Ast.Allow i -> update true i
    | Deny i -> update false i
  );
  match output with
  | Escaped -> print_endline w#to_escaped
  | Raw -> print_string w#to_string
  | Table -> Demo.dump lang w


let () =
  let info =
    let doc = "logonHours AD attribute manipulation tool" in
    Cmd.info "logonHours" ~doc
  in
  exit @@ Cmd.eval @@ Cmd.v info @@ Term.(
    const main $ inp $ bias $ lang $ output $ import $ class_ $ commands
  )
