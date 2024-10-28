open Core

type documentable_data =
  | Function of string (* declaration *)
  | Constant of string (* definition *)
  | ADT of string (* each line is a field/declaration *)
[@@deriving show]

val parse_dd : documentable_data -> documentable_data

module Doc : sig
  type t = string * documentable_data

  val to_text : t -> string list * documentable_data
end

module FileLexer : sig
  open Syntax

  type t [@@deriving show]

  val init : string list -> t
  val next_element : t -> t * (int * syntax) option
  val next_string : t -> t
  val length : t -> int
end

val extract_comments : In_channel.t -> Doc.t Queue.t
