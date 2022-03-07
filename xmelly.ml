type attr = string * string

type t =
  | Element of string * attr list * t list
  | Text of string

open Tokeniser

let fail (lc : lc_channel) = fail_with lc "invalid input"

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
    | PIEnd -> []
    | _ -> fail_with lc "expecting ident or '?>' in preamble"
  in
  match_nonblank_token (Ident "xml") lc |> ignore;
  attrs () |> ignore

let rec discard_pi (lc : lc_channel) =
  match next_nonblank_token lc with
  | PIEnd -> ()
  | Eof -> fail_with lc "missing '?>' to finish a process instruction"
  | _ -> discard_pi lc

let rec match_node (lc : lc_channel) (tag : string) : t =
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
        match_token GT lc |> ignore;
        ([], true)
    | _ -> fail_with lc "expecting attribute name or '>' or '/' after tag opening"
  in
  let (attrs, closed) = attrs () in
  if closed
  then Element (tag, attrs, [])
  else Element (tag, attrs, node_kids tag lc)
and node_kids tag (lc : lc_channel) =
  let is_text = function '<' | ' ' | '\t' | '\r' | '\n' -> false | _ -> true in
  let t = consume lc is_text in
  let kids_after_lt () =
    match next_token lc with
    | Slash ->
        match_token (Ident tag) lc |> ignore;
        match_nonblank_token GT lc |> ignore;
        []
    | Ident t ->
        let child = match_node lc t in
        child :: node_kids tag lc
    | _ -> fail_with lc "expecting tag name or '/' after '<'"
  in
  let merge_text t kids =
    match kids with
    | Text tt :: ks -> Text (t ^ " " ^ tt) :: ks
    | ks -> Text t :: ks
  in
  let kids =
    match next_token lc with
    | LT -> kids_after_lt ()
    | PIStart -> discard_pi lc; node_kids tag lc
    | Space -> node_kids tag lc
    | CData x -> merge_text x (node_kids tag lc)
    | _ -> fail_with lc "expecting '<', '<?' or spaces inside a tag"
  in 
  if t = "" 
  then kids
  else merge_text t kids

let parse (ic : in_channel) : t =
  let lc : lc_channel = (ref 1, ref [], ic) in
  let _ = match next_nonblank_token lc with
    | LT -> ()
    | PIStart ->
        match_preamble lc;
        match_nonblank_token LT lc |> ignore
    | _ -> fail_with lc "expecting tag or <? at the beginning of the file"
  in
  match_ident lc |> match_node lc

