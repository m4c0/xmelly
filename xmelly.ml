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

type lc_channel = int ref * char list ref * in_channel

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

let rec next_nonblank_token (lc : lc_channel) : token =
  match next_token lc with
  | Space -> next_nonblank_token lc
  | x -> x

let match_token (tk : token) (lc : lc_channel) : token =
  let t = next_token lc in
  if t <> tk then fail lc else t

let match_nonblank_token (tk : token) (lc : lc_channel) : token =
  let t = next_nonblank_token lc in
  if t <> tk then fail lc else t

let match_ident (lc : lc_channel) : string =
  match next_nonblank_token lc with
  | Ident x -> x
  | _ -> fail lc

let match_value (lc : lc_channel) : string =
  match next_nonblank_token lc with
  | Value x -> x
  | _ -> fail lc

let match_preamble (lc : lc_channel) : unit =
  let rec attrs () =
    match next_nonblank_token lc with
    | Ident k ->
        let _ = match_nonblank_token EQ lc in
        let v = match_value lc in
        (k, v) :: attrs ()
    | QMark ->
        match_token GT lc |> ignore;
        []
    | _ -> fail lc
  in
  match_nonblank_token LT lc |> ignore;
  match_token QMark lc |> ignore;
  match_nonblank_token (Ident "xml") lc |> ignore;
  attrs () |> ignore

let rec match_node (tag : string) (lc : lc_channel) : t =
  let rec attrs () =
    match next_nonblank_token lc with
    | Ident k ->
        let _ = match_nonblank_token EQ lc in
        let v = match_value lc in
        let (kvs, closed) = attrs () in
        ((k, v) :: kvs, closed)
    | GT ->
        ([], false)
    | Slash -> 
        match_token LT lc |> ignore;
        ([], true)
    | _ -> fail lc
  in
  let (attrs, closed) = attrs () in
  if closed
  then Element (tag, attrs, [])
  else Element (tag, attrs, node_kids tag lc)
and node_kids tag (lc : lc_channel) =
  match next_nonblank_token lc with
  | LT -> begin
    match next_token lc with
    | Slash ->
        match_token (Ident tag) lc |> ignore;
        match_nonblank_token GT lc |> ignore;
        []
    | Ident t ->
        let child = match_node t lc in
        child :: node_kids tag lc
    | _ -> fail lc
  end
  | Ident t -> Text t :: node_kids tag lc
  | _ -> fail lc

let parse (ic : in_channel) : t =
  let lc : lc_channel = (ref 1, ref [], ic) in
  match_preamble lc;
  let _ = match_nonblank_token LT lc in
  let tag = match_ident lc in
  match_node tag lc

