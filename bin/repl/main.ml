module Repl (Frontend: Core.REPL_FRONTEND) = struct
  (* for now only read single line. todo: in parse stage if unclosed structure accept further input *)
    
  let run () = 
    let rec repl ctx =
      print_string "λ> ";
      flush stdout;
      let input = try read_line () with End_of_file -> "" in
      match Frontend.eval_source ctx input with
      | Ok (ctx, echo) ->
        print_endline echo;
        repl ctx (* new context *)
      | Error {msg; start; stop} ->
        let output =
          "Error at " ^ string_of_int start ^ ":" ^ string_of_int stop ^ "\n" ^ msg in
        print_endline output;
        repl ctx (* all changes discarded *)
    in
    repl Frontend.empty_ctx
end

module LambdaRepl = Repl (Core.Repl_frontend (Lambda))

let print_greeting () =
  print_endline "Type Theory Playground 0.0.1";
  print_endline ""

let print_usage () =
  let names = ["lambda"] in
  print_endline "Usage (stale): ";
  print_endline ("    typec [ " ^ String.concat " | " names ^ " ] [ <path> ... ]: start REPL in the specified type system, optionally importing modules");
  print_endline "    typec <file> ...: start REPL by recognizing the module extension name"

let () =
  print_greeting ();
  match if Array.length Sys.argv < 2 then (
    print_endline "Defaults to Untyped Lambda Calculus";
    print_endline "";
    "lambda")
  else Sys.argv.(1) with
  | "lambda" -> LambdaRepl.run ()
  | "system-f" -> print_endline "Not yet implemented"
  | _ -> print_usage ()
