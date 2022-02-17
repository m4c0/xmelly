type attr = string * string

type t =
  | Element of string * attr list * t list
  | Text of string

val parse : in_channel -> t
