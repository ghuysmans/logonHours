open LogonHours

type lang =
  | English
  | French

let dump lang w =
  let days, f =
    let open Day in
    match lang with
    | English -> american, to_string
    | French -> european, to_french
  in
  let print_stats ch (ct, max) =
    Printf.fprintf ch "%2d h,%3d %%" ct (ct * 100 / max)
  in
  Printf.printf "    0123456789AB0123456789AB\n";
  let l = w#to_local in
  let total = ref 0 in
  days |> List.iter (fun d ->
    Printf.printf "%s " (f d);
    let ct = ref 0 in
    for h = 0 to 23 do
      Printf.printf "%c" (if l.(Day.to_int d * 24 + h) then (incr ct; '1') else '0')
    done;
    Printf.printf " (%a)\n" print_stats (!ct, 24);
    total := !total + !ct
  );
  Printf.printf "\nTotal: %a\n" print_stats (!total, 7 * 24)

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
    let from, until =
      Scanf.sscanf range "%d:%d-%d:%d" (fun h m h' m' -> (h, m), (h', m'))
    in
    Some {day; from; until}
  ) in
  let c = Arg.conv (parser, fun _ _ -> () (* FIXME? *)) in
  Arg.(value & opt_all c [] & info ~doc ["i"; "interval"])

let main inp bias lang clear output intervals =
  let w =
    match inp with
    | None -> make ~bias
    | Some f -> of_string ~bias (really_input_string (open_in f) 21)
  in
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
  | Table -> dump lang w


let () =
  Term.(exit @@ eval @@
    let doc = "logonHours AD attribute manipulation tool" in
    const main $ inp $ bias $ lang $ clear $ output $ intervals,
    info "logonHours" ~doc
  )
