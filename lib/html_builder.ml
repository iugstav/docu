open Tyxml
open Syntax
open Go
open Core

type t =
  { lexer : Go.FileLexer.t
  ; current : (int * syntax) option
  ; definition : documentable_data
  }

let syntax_to_html = function
  | Text s -> Html.txt s
  | Link (url, visual) -> Html.(a ~a:[ a_href url ] [ txt visual ])
  | InlineCode content -> Html.(code [ txt content ])
;;

let convert_sq (l : (int * syntax list) list) =
  List.map l ~f:(fun (_, s_list) -> List.map s_list ~f:syntax_to_html)
;;

let next builder =
  let lexer, current = FileLexer.next_element builder.lexer in
  { lexer; current; definition = builder.definition }
;;

let rec init lexer definition = { lexer; current = None; definition }

and to_paragraph builder =
  let queue = build_syntax_q builder in
  group queue
  |> convert_sq
  |> List.map ~f:(fun l ->
    Html.p (List.fold l ~init:[] ~f:(fun acc tag -> acc @ [ tag ])))

and build_syntax_q builder =
  let queue = Queue.create () in
  let rec fill_queue builder =
    let builder = next builder in
    match builder.current with
    | Some el ->
      Queue.enqueue queue el;
      fill_queue builder
    | None -> ()
  in
  fill_queue builder;
  queue

and group queue =
  let rec relate key value = function
    | [] -> [ key, [ value ] ]
    | (k, v) :: tail ->
      if k = key then (k, value :: v) :: tail else (k, v) :: relate key value tail
  in
  List.fold (Queue.to_list queue) ~init:[] ~f:(fun acc (i, value) -> relate i value acc)
;;

module DocumentCreator = struct
  open Soup

  type 'a t =
    { current : 'a Html.elt list Queue.t
    ; mutable insert_position : int
    ; template_name : string
    }

  let init templ el =
    match templ with
    | "" ->
      Ok { current = el; insert_position = 0; template_name = "template/index.html" }
    | str when not (String.is_suffix str ~suffix:".html") ->
      Error (Printf.printf "Invalid file format '%s'\n" str)
    | template -> Ok { current = el; insert_position = 0; template_name = template }
  ;;

  let rec build t =
    let file_content = read_html_template t in
    let tags = Queue.map t.current ~f:elements_to_string |> Queue.to_list |> flatten in
    let rec insert = function
      | [] -> raise_s (Sexp.of_string "could not find document tag")
      | hd :: tl ->
        if contains hd "<div data-docu>" <> -1 then (hd :: tags) @ tl else hd :: insert tl
    in
    let file_content = insert file_content in
    Stdlib.Out_channel.with_open_gen
      [ Open_text; Open_wronly; Open_creat ]
      0o744
      "output/docu.html"
      (fun oc ->
         (*Out_channel.seek oc (Int64.of_int t.insert_position);*)
         (*Out_channel.output_char oc '\n';*)
         Out_channel.output_lines oc file_content)

  and read_html_template t =
    let rec buf_read ic buf acc =
      let operation = In_channel.input_buffer ic buf ~len:4092 in
      let element_position = contains (Buffer.contents buf) "<div data-docu>" + 15 in
      match operation with
      | None -> t.insert_position <- element_position + acc
      | Some _ ->
        if element_position = -1
        then
          if Buffer.length buf < 4092
          then raise_s (Sexp.of_string "template tag not found in document")
          else (
            let new_acc = acc + Buffer.length buf in
            Buffer.clear buf;
            buf_read ic buf new_acc)
        else t.insert_position <- element_position + acc
    in
    In_channel.with_file t.template_name ~f:(fun ic ->
      let buf = Buffer.create 4092 in
      buf_read ic buf 0;
      In_channel.seek ic 0L;
      In_channel.input_lines ic)

  and contains str sub =
    let regex = Str.regexp_string sub in
    match Str.search_forward regex str 0 with
    | pos -> pos
    | exception _ -> -1

  and elements_to_string l =
    List.map l ~f:(fun tag -> Format.asprintf "%a" (Tyxml.Html.pp_elt ()) tag)

  and flatten l =
    let rec aux (acc : 'a list) = function
      | [] -> List.rev acc
      | x :: tl -> aux (List.rev_append x acc) tl
    in
    aux [] l
  ;;
end
