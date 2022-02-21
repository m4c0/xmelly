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

let rec take_n (n : int) (lc : lc_channel) : char list =
  match n with
  | 0 -> []
  | nn ->
      match take lc with
      | None -> []
      | Some c -> c :: take_n (nn - 1) lc

let undo ((_, cl, _) : lc_channel) (c : char) (res : 'a) : 'a = 
  cl := c :: !cl;
  res

let rec undo_n (lc : lc_channel) (cs : char list) (res : 'a) : 'a =
  match cs with
  | [] -> res
  | c :: cc -> undo_n lc cc res |> undo lc c

let rec consume (lc : lc_channel) (fn : char -> bool) : string =
  match take lc with
  | None -> ""
  | Some c -> 
      if fn c
      then ((String.make 1 c) ^ (consume lc fn))
      else undo lc c ""

let fail_with (lc : lc_channel) (msg : string) =
  let rem = consume lc (fun c -> c <> '\n') in
  let (line, _, _) = lc in
  let msg = Printf.sprintf "line %d: %s near [%s]" (!line - 1) msg rem in
  failwith msg

let fail (lc : lc_channel) = fail_with lc "invalid input"

let rec consume_until_endcomment (lc : lc_channel) : token =
  match take_n 3 lc with
  | ['-';'-';'>'] -> Space
  | [_;a;b] ->
      undo lc b () |> undo lc a;
      consume_until_endcomment lc
  | _ -> failwith "comment wasn't closed"

let next_token (lc : lc_channel) : token =
  match take lc with
  | Some('<') -> (
      match take_n 3 lc with
      | ['!';'-';'-'] -> consume_until_endcomment lc
      | '!' :: xs -> undo_n lc xs LT (* TODO: doctype *)
      | xs -> undo_n lc xs LT
  )
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


