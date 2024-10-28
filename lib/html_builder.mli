type t

val init : Go.FileLexer.t -> Go.documentable_data -> t
val to_paragraph : t -> [> Html_types.p ] Tyxml.Html.elt list
val build_syntax_q : t -> (int * Syntax.syntax) Core.Queue.t

module DocumentCreator : sig
  type 'a t

  val init : string -> 'a Tyxml.Html.elt list Base.Queue.t -> ('a t, unit) result
  val build : 'a t -> unit
end
