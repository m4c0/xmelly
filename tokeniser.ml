type token =
  | LT
  | GT
  | EQ
  | Slash
  | QMark
  | Space
  | Ident of string
  | Value of string
  | Eof

type lc_channel = int ref * char list ref * in_channel

let is_ident : char -> bool = function
  | '<'
  | '>'
  | '?'
  | '='
  | '/'
  | ' '
  | '\n'
  | '"' -> false
  | _ -> true

let safe_input_char (ic : in_channel) : char option =
  try Some (input_char ic)
  with End_of_file -> None

let take ((line, cl, ic) : lc_channel) : char option =
  match !cl with
  | c :: cs -> cl := cs; Some c
  | [] ->
      let co = safe_input_char ic in
      let is_nl = match co with
        | Some '\n' -> true
        | Some _
        | None -> false
      in
      line := if is_nl then !line + 1 else !line;
      co

let undo ((_, cl, _) : lc_channel) (c : char) : unit = cl := c :: !cl

let rec consume (lc : lc_channel) (fn : char -> bool) : string =
  match take lc with
  | None -> ""
  | Some c -> 
      if fn c
      then ((String.make 1 c) ^ (consume lc fn))
      else (undo lc c; "")

let fail (lc : lc_channel) =
  let rem = consume lc (fun c -> c <> '\n') in
  let (line, _, _) = lc in
  let msg = Printf.sprintf "line %d: invalid input near %s" (!line - 1) rem in
  failwith msg

let next_token (lc : lc_channel) : token =
  match take lc with
  | Some('<') -> LT
  | Some('>') -> GT
  | Some('/') -> Slash
  | Some('?') -> QMark
  | Some('=') -> EQ
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


