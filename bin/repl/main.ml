module Repl (Frontend: Core.REPL_FRONTEND) = struct
  (* for now only read single line. todo: in parse stage if unclosed structure accept further input *)
    
  let run () = 
    let rec repl ctx =
      print_string "λ> ";
      flush stdout;
      let input = try read_line () with End_of_file -> "" in
      match Frontend.eval_statements ctx input with
      | Ok (ctx, echo) ->
        print_string echo;
        repl ctx (* new context *)
      | Error {msg; start; stop} ->
        let output =
          "Error at " ^ string_of_int start ^ ":" ^ string_of_int stop ^ "\n    " ^ msg in
        print_endline output;
        repl ctx (* all changes discarded *)
    in
    repl Frontend.empty_ctx
end

module LambdaArrowRepl = Repl (Core.Repl_frontend (LambdaArrow))

let print_usage () =
  let names = ["lambda-arrow"] in
  print_endline "Usage: ";
  print_endline ("    typec [ " ^ String.concat " | " names ^ " ] [ <path> ... ]: start REPL in the specified type system, optionally importing modules");
  print_endline "    typec <path> ...: start REPL by recognizing the module extension name"

let () =
if Array.length Sys.argv < 2 then print_usage () else
  match Sys.argv.(1) with
  | "lambda-arrow" -> LambdaArrowRepl.run ()
  (* | "system-f" -> () *)
  | _ -> print_usage ()
