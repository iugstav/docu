open Docu
open Core

let read_file fname = In_channel.with_file fname ~f:(fun ic -> Go.extract_comments ic)

let () =
  let comm = read_file "samples/main.go" in
  let q =
    Core.Queue.map comm ~f:(fun doc ->
      let comments, typo = Go.Doc.to_text doc in
      let lexer = Go.FileLexer.init comments in
      let builder = Html_builder.init lexer typo in
      Html_builder.to_paragraph builder)
  in
  let dc =
    Html_builder.DocumentCreator.init "template/index.html" q
  in
  match dc with
  | Ok t -> Html_builder.DocumentCreator.build t
  | Error _ -> failwith "Não deu pra inicializar paizão"
;;
