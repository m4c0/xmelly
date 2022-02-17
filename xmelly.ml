type attr = string * string

type t =
  | Element of string * attr list * t list
  | Text of string

let parse (_ : in_channel) : t =
  Element ("tag", [("a", "1"); ("b", "2")], [
    Element ("another", [("a", "1")], []);
    Element ("yet", [("a", "1")], [
      Text "another"; 
      Text "test"]); 
    Text "text"])

