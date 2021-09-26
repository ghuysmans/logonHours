open Js_of_ocaml
open Smartschool

let weekday {Glical.Datetime.year; month; day; _} =
  match (new%js Js.date_day year (month - 1) day)##getDay with
  | 0 -> LogonHours.Day.Sunday
  | 1 -> Monday
  | 2 -> Tuesday
  | 3 -> Wednesday
  | 4 -> Thursday
  | 5 -> Friday
  | 6 -> Saturday
  | _ -> failwith "getDay"

let process inp add_course print_attribute =
  My_form.get_files inp |>
  List.iter (fun f ->
    let r = new%js File.fileReader in
    r##.onload := Dom.handler (fun _ ->
      begin match File.CoerceTo.string r##.result |> Js.Opt.to_option with
      | None -> prerr_endline "readAsText returned a non-string"
      | Some raw ->
        let w = LogonHours.make ~bias:1 in
        let l = Js.to_string raw |> Ical.parse in
        List.map (fun {Ical.subject; class_; _} -> subject, class_) l |>
          List.sort_uniq compare |>
          List.iter (fun (subject, class_) -> add_course subject class_);
        List.sort compare l |>
          List.iter (fun {Ical.event = {dtstart; dtend; _}; _} ->
            let d = weekday dtstart in
            if d = weekday dtend then
              LogonHours.interval (dtstart.hours, dtstart.minutes)
                                  (dtend.hours, dtend.minutes) |>
              List.iter (w#set d)
            else
              failwith "a course can't span two days"
          );
        print_attribute w#to_escaped
      end;
      Js._true
    );
    r##readAsText f
  )


let () =
  Js.export "reader" (object%js
    method extract =
      let sel = Option.get Dom_html.(getElementById_coerce "f" CoerceTo.input) in
      let ul = Option.get Dom_html.(getElementById_coerce "courses" CoerceTo.ul) in
      ul##.innerHTML := Js.string "";
      let add_course subject class_ =
        let li = Dom_html.(createLi document) in
        let s = Printf.sprintf "%s en %s" subject class_ in
        li##.textContent := Js.(some (string s));
        ignore @@ ul##appendChild (li :> Dom.node Js.t)
      in
      let pre = Option.get Dom_html.(getElementById_coerce "blob" CoerceTo.pre) in
      let print_attribute x = pre##.textContent := Js.(some (string x)) in
      process sel add_course print_attribute;
      Js._false
  end)
