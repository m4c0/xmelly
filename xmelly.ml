type attr = string * string

type t =
  | Element of string * attr list * t list
  | Text of string

type token =
  | LT
  | GT
  | EQ
  | Slash
  | QMark
  | Excl
  | Space
  | Ident of string
  | Value of string
  | Eof

type lc_channel = char list ref * in_channel

let is_ident : char -> bool = function
  | '<'
  | '>'
  | '?'
  | '='
  | '/'
  | '!'
  | ' '
  | '\n'
  | '"' -> false
  | _ -> true

let safe_input_char (ic : in_channel) : char option =
  try Some (input_char ic)
  with End_of_file -> None

let take ((cl, ic) : lc_channel) : char option =
  match !cl with
  | c :: cs -> cl := cs; Some c
  | [] -> safe_input_char ic

let undo ((cl, _) : lc_channel) (c : char) : unit = cl := c :: !cl

let rec consume (lc : lc_channel) (fn : char -> bool) : string =
  match take lc with
  | None -> ""
  | Some c -> 
      if fn c
      then ((String.make 1 c) ^ (consume lc fn))
      else (undo lc c; "")

let next_token (lc : lc_channel) : token =
  match take lc with
  | Some('<') -> LT
  | Some('>') -> GT
  | Some('/') -> Slash
  | Some('?') -> QMark
  | Some('=') -> EQ
  | Some('!') -> Excl
  | Some(' ')
  | Some('\n') -> 
      consume lc (function
        | ' ' | '\n' -> true
        | _ -> false) |> ignore;
      Space
  | Some('"') -> 
      let v = consume lc (fun c -> c <> '"') in
      take lc |> ignore;
      Value v 
  | Some(c) -> 
      let s = String.make 1 c in
      let v = consume lc is_ident in
      Ident (s ^ v)
  | None -> Eof

let parse (ic : in_channel) : t =
  let lc : lc_channel = (ref [], ic) in
  let n () = match next_token lc with
  | LT -> Some "LT"
  | GT -> Some "GT"
  | Slash -> Some "Slash"
  | EQ -> Some "EQ"
  | QMark -> Some "QMark"
  | Excl -> Some "Excl"
  | Space -> Some "Space"
  | Ident x -> Some ("Ident " ^ x)
  | Value x -> Some ("Value " ^ x)
  | Eof -> None
  in
  let rec nn () = match n () with
  | Some x -> (Text x) :: nn ()
  | None -> []
  in
  Element ("root", [], (nn ()))

