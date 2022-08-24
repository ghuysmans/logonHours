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
