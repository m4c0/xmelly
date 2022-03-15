type token =
  | LT
  | GT
  | PIStart
  | PIEnd
  | EQ
  | Slash
  | Space
  | Ident of string
  | Value of string
  | CData of string
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
  | '\r'
  | '\'' -> false
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
        | Some '\n' | Some '\r' -> true
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

let consume (lc : lc_channel) (fn : char -> bool) : string =
  let rec aux acc = 
    match take lc with
    | None -> acc 
    | Some c when fn c -> acc ^ (String.make 1 c) |> aux
    | Some c -> undo lc c acc 
  in
  aux ""

let fail_with (lc : lc_channel) (msg : string) =
  let rem = consume lc (fun c -> c <> '\n') in
  let (line, _, _) = lc in
  let msg = Printf.sprintf "line %d: %s near [%s]" (!line - 1) msg rem in
  failwith msg

let rec consume_until_endcomment (lc : lc_channel) : token =
  match take_n 3 lc with
  | ['-';'-';'>'] -> Space
  | [_;a;b] ->
      undo lc b () |> undo lc a;
      consume_until_endcomment lc
  | _ -> failwith "comment wasn't closed"

let rec consume_until_endcdata (lc : lc_channel) : char list =
  match take_n 3 lc with
  | [']';']';'>'] -> []
  | [a;b;c] ->
      undo lc c () |> undo lc b;
      a :: consume_until_endcdata lc
  | _ -> failwith "cdata wasn't closed"

let consume_cdata (lc : lc_channel) : token =
  match take_n 5 lc with
  | ['D';'A';'T';'A';'['] ->
      let v = consume_until_endcdata lc |> List.to_seq |> String.of_seq in
      CData v
  | x ->
      undo_n lc x ();
      fail_with lc "expecting CDATA"

let consume_ident (c : char list) (lc : lc_channel) : token =
  let s = List.map (String.make 1) c |> String.concat "" in
  let v = consume lc is_ident in
  Ident (s ^ v)

let next_token (lc : lc_channel) : token =
  match take lc with
  | Some('<') -> (
      match take_n 3 lc with
      | ['!';'-';'-'] -> consume_until_endcomment lc
      | ['!';'[';'C'] -> consume_cdata lc
      | '!' :: xs -> undo_n lc xs LT (* TODO: doctype *)
      | '?' :: xs -> undo_n lc xs PIStart
      | xs -> undo_n lc xs LT
  )
  | Some('>') -> GT
  | Some('/') -> Slash
  | Some('?') -> (
      match take lc with
      | Some('>') -> PIEnd
      | Some(x) -> consume_ident ['?';x] lc
      | None -> Ident("?")
  )
  | Some('=') -> EQ
  | Some(' ' | '\t' | '\r' | '\n') ->
      consume lc (function
        | ' ' | '\t' | '\r' | '\n' -> true
        | _ -> false) |> ignore;
      Space
  | Some('\'') -> 
      let v = consume lc (fun c -> c <> '\'') in
      take lc |> ignore;
      Value v 
  | Some('"') -> 
      let v = consume lc (fun c -> c <> '"') in
      take lc |> ignore;
      Value v 
  | Some(c) -> consume_ident [c] lc
  | None -> Eof

