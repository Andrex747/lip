module T = ANSITerminal
open Life.Main

let _ = match Array.length(Sys.argv) with
    
  | 3 ->
    let rule_str = Sys.argv.(1) in
    let n_rounds = int_of_string Sys.argv.(2) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();

    (* Parse la regola usando Rule_parser.parse *)
    let rule = parse rule_str in
    let rule_pair = match rule with
        | Rule (s, b) -> (s, b)  
        | Seq s -> (s, [])       
      in

    (* Inizializza il mondo con la regola *)
    let w = loop init_w  n_rounds rule_pair in
    display w;

    ignore (read_line ());
    T.restore_cursor ();
    print_newline ()


  (* wrong usage *)
  | _ -> failwith "Usage: dune exec life n_rounds"
