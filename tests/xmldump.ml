let () = 
  let open Xmelly in
  let rec dump lpad node =
    print_string lpad;
    match node with
    | Text x -> print_endline x
    | Element (x, a, l) ->
        let dump_kv (k, v) =
          print_string " ";
          print_string k;
          print_string "=";
          print_string v
        in
        let newpad = lpad ^ "  " in
        print_string x;
        List.iter dump_kv a;
        print_endline ":";
        List.iter (dump newpad) l
  in
  parse stdin |> (dump "")
