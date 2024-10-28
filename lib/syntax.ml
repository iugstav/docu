  type syntax =
    | Link of string * string (* url, url visual replacement *)
    | Text of string
    | InlineCode of string
  [@@deriving show]
