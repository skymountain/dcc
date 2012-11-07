module Env = Syntax.Env

exception Exit

let err s = print_endline s

let rec read_eval_print prompt fun_lexbuf env err =
  print_string prompt;
  flush stdout;
  try
    let prog = Parser.toplevel Lexer.main (Lexing.from_function fun_lexbuf) in
    let (id ,v, newenv) = Eval.eval_program env prog in
    Printf.printf "val %s = %s\n" id (Eval.string_of_exval v);
    read_eval_print prompt fun_lexbuf newenv err
  with
  | e -> begin
      let co = BatIO.output_string () in
      BatPrintexc.print co e;
      BatIO.nwrite co "\n";
      err (BatIO.close_out co);
      read_eval_print prompt fun_lexbuf env err
    end

let refill_buffer ch =
  let rec fill_buff dst idx len =
    if len = idx then idx
    else begin
      try
        let c = input_char ch in
        dst.[idx] <- c;
        if c = '\n' then idx+1
        else fill_buff dst (idx+1) len
      with
        End_of_file -> idx
    end
  in
  let body buf len = fill_buff buf 0 len in
  body

let main () =
  let files = ref [] in
  let interact = ref None in
  let _ =
    Arg.parse ["-i", Arg.Unit (fun _ -> interact := Some true), "interact with interpretor"]
      (fun f -> files := f::!files; if !interact = None then interact := Some false else ())
      "Usage: lambda [file...]"
  in
  let interact = match !interact with None -> true | Some b -> b in
  let ferr s = err s; raise Exit in
  let files =
    List.fold_left
      (fun xs file ->
         ((try open_in file with Sys_error _ -> err (Printf.sprintf "%s: No such file." file); raise Exit), ferr, Some file, "")::xs)
      [] !files
  in
  let ins = List.append files (if interact then [(stdin, (fun s -> err s), None, "> ")] else []) in
  List.fold_left
    (fun env (ichann, err, file, prompt) ->
       (* print file name *)
       (match file with
          None -> ()
        | Some file -> print_endline (Printf.sprintf "will load %s" file));
       read_eval_print prompt (refill_buffer ichann) env err)
    Env.empty
    ins

let _ = try ignore (main ()) with Exit -> ();
