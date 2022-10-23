(* taken from jsoo/form.mli! *)
open Js_of_ocaml
open Js
open Dom_html

(* TODO expose *)
class type file_input =
  object
    inherit inputElement

    method files : File.fileList t optdef readonly_prop 

    method multiple : bool optdef readonly_prop
  end

(* broken copy-pasta *)
let get_input_val ?(get = false) (elt : inputElement t) =
    let name = to_string elt##.name in
    let value = elt##.value in
    match to_bytestring elt##._type##toLowerCase with
    | "checkbox" | "radio" ->
        if to_bool elt##.checked then [ name, `String value ] else []
    | "submit" | "reset" -> []
    | "text" | "password" -> [ name, `String value ]
    | "file" -> (
        if get
        then [ name, `String value ]
        else
          let elt : file_input t = Unsafe.coerce elt in
          match Optdef.to_option elt##.files with
          | None -> []
          | Some list -> (
              if list##.length = 0
              then (print_endline "empty"; [ name, `String (Js.string "") ])
              else
                match Optdef.to_option elt##.multiple with
                | None | Some false -> (
                    match Opt.to_option (list##item 0) with
                    | None -> []
                    | Some file -> [ name, `File file ])
                | Some true ->
                    List.filter_map
                      (fun f ->
                        match Opt.to_option f with
                        | None -> None
                        | Some file -> Some (name, `File file))
                      (Array.to_list (Array.init list##.length (fun i -> list##item i)))))
    | _ -> print_endline "wtf?"; [ name, `String value ]

(* TODO add? *)
let get_files (elt : inputElement t) =
  get_input_val elt |>
  List.filter_map (function
    | (_, `File f) -> Some f
    | _ -> print_endline "skipped"; None
  )
