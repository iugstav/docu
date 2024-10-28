open Core

type documentable_data =
  | Function of string (* declaration *)
  | Constant of string (* definition *)
  | ADT of string (* each line is a field/declaration *)
[@@deriving show]

let parse_dd = function
  | Function s ->
    Function (String.rstrip s ~drop:(fun c -> Char.(equal c '{' || equal c ' ')))
  | Constant c -> Constant c
  | ADT t -> ADT (String.rstrip t ~drop:(fun c -> Char.(equal c '{' || equal c ' ')))
;;

let first_word str =
  let rec traverse_until_space index =
    match str.[index] with
    | ' ' -> index
    | _ -> traverse_until_space (index + 1)
  in
  let index = traverse_until_space 0 in
  Stdlib.String.sub str 0 index
;;

let kword_to_dd data =
  match first_word data with
  | "func" -> Some (Function data)
  | "const" -> Some (Constant data)
  | "type" -> Some (ADT data)
  | _ -> None
;;

let extract_comments ic =
  let open Core in
  let func_docs = Queue.create () in
  let comments = Queue.create () in
  let rec read lnum =
    match In_channel.input_line ic with
    | Some line ->
      if String.length line < 2
      then read (lnum + 1)
      else if Char.equal line.[0] '/'
      then (
        Queue.enqueue comments (line, lnum);
        read (lnum + 1))
      else (
        match Queue.last comments with
        | Some (_, number) ->
          (* if the last inserted comment isn't at the previous line *)
          if number <> lnum - 1
          then (
            Queue.clear comments;
            read (lnum + 1))
          else (
            (* since the line isn't a comment, it starts identifying which data is present here *)
            let typo = kword_to_dd line in
            match typo with
            | None ->
              Queue.clear comments;
              read (lnum + 1)
            | Some dd ->
              let grouped_comments =
                Queue.fold comments ~init:"" ~f:(fun acc (comment, _) ->
                  acc ^ comment ^ "\n")
              in
              Queue.enqueue func_docs (grouped_comments, dd));
          Queue.clear comments
        | None -> ());
      read (lnum + 1)
    | None -> ()
  in
  read 1;
  func_docs
;;

module Doc = struct
  open Core

  type t = string * documentable_data

  let to_text ((comment, datatype) : t) =
    let split_on_empty l =
      let rec aux acc current = function
        | [] -> List.rev (List.rev current :: acc)
        | "" :: rest -> aux (List.rev current :: acc) [] rest
        | x :: rest -> aux acc (x :: current) rest
      in
      aux [] [] l
    in
    let elements =
      String.split_lines comment
      |> List.map ~f:(fun comm ->
        String.lstrip ~drop:(fun c -> Char.equal c '/') comm
        |> String.strip ~drop:(fun c -> Char.equal c ' '))
      |> split_on_empty
      |> List.map ~f:(fun list -> String.concat ~sep:" " list)
      |> List.fold ~init:"" ~f:(fun acc s -> acc ^ s ^ "\n")
      |> String.split_lines
    in
    elements, datatype
  ;;
end

module FileLexer = struct
  open Core
  open Syntax

  type t =
    { input : string list
    ; which : int
    ; position : int
    ; ch : char option
    }
  [@@deriving show]

  let is_input_empty l = List.exists l ~f:String.is_empty
  let which_string lexer = List.nth_exn lexer.input lexer.which

  let init input =
    if is_input_empty input
    then { input; which = 0; position = 0; ch = None }
    else
      { input
      ; which = 0
      ; position = 0
      ; ch = Some (String.get (Stdlib.Option.get (List.hd input)) 0)
      }
  ;;

  let rec next_element lexer =
    match lexer.ch with
    | None ->
      let lex = next_string lexer in
      if lex.which = List.length lex.input then lex, None else next_element lex
    | Some ch ->
      let lexer, element =
        match ch with
        | '`' -> between_ticks lexer
        | '[' -> between_square_brackets lexer
        | c when is_text c -> read_text lexer
        | c -> failwith (Printf.sprintf "unknown '%c'" c)
      in
      lexer, Some (lexer.which, element)

  and next lex =
    if lex.position = String.length (which_string lex) - 1
    then { lex with ch = None }
    else (
      let pos = lex.position + 1 in
      { lex with position = pos; ch = Some (String.get (which_string lex) pos) })

  and next_string lex =
    let which = lex.which + 1 in
    { lex with which; position = 0; ch = Some (String.get (which_string lex) 0) }

  and search lex cond =
    let rec loop lexer = if cond lexer.ch then loop (next lexer) else lexer in
    let lexer = loop lex in
    lexer, lexer.position

  and look_while lex cond =
    let start = lex.position in
    let lexer, final =
      search lex (fun ch ->
        match ch with
        | Some character -> cond character
        | None -> false)
    in
    lexer, String.sub (which_string lexer) ~pos:start ~len:(final - start)

  and read_text lex =
    let initial = lex.position in
    let lexer, pos =
      search lex (fun ch ->
        match ch with
        | Some c -> is_text c
        | None -> false)
    in
    let extra = if is_text (String.get (which_string lexer) pos) then 1 else 0 in
    let data =
      String.sub (which_string lexer) ~pos:initial ~len:(pos - initial + extra)
    in
    lexer, Text data

  and between_square_brackets lex =
    let lexer = next lex in
    let lexer, url = look_while lexer not_rsquare in
    let lexer = next lexer in
    let lexer, replacement = between_paren lexer in
    let lexer = next lexer in
    lexer, Link (url, replacement)

  and between_paren lex =
    let lexer = next lex in
    look_while lexer not_rparen

  and between_ticks lex =
    let lexer = next lex in
    let lexer, data = look_while lexer not_tick in
    let lexer = next lexer in
    lexer, InlineCode data

  and is_multiline s =
    match String.find s ~f:(fun c -> Char.equal c '\n') with
    | Some _ -> true
    | None -> false

  and length lexer = List.length lexer.input
  and is_text ch = not_rsquare ch && (not (Char.equal ch '[')) && not_tick ch
  and not_rsquare ch = not (Char.equal ch ']')
  and not_tick ch = not (Char.equal ch '`')
  and not_rparen ch = not (Char.equal ch ')')
end
