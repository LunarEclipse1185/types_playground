type errmsg = { msg: string; start: int; stop: int; }

module type SYNTAX = sig
  type expr
  type tyexpr
  type statement
  val ast_from_source: string -> (statement list, errmsg) result
end

module type SEMANTICS = sig
  include SYNTAX
  type tm
  type ty
  
  type ctx
  val empty_ctx: ctx

  val parse_tm: ctx -> expr -> (tm, errmsg) result
  val parse_ty: ctx -> tyexpr -> (ty, errmsg) result
  
  val ty_eq: ty -> ty -> bool
  val string_of_ty: ty -> string
  val string_of_tm: tm -> string
end

module type THEORY = sig
  include SEMANTICS
  
  val infer: ctx -> tm -> (ty, errmsg) result
  
  type eval_result_t = (ctx * string, errmsg) result
  
  val eval_dir: ctx -> string -> expr option -> eval_result_t
  
  val eval_dir_deny: ctx -> string -> expr option -> eval_result_t
  
  val eval: (ctx -> string -> expr option -> eval_result_t) ->
            ctx -> statement -> eval_result_t
  (* interface for repl and interpreter *)
  
  (* val import_module: string -> string -> (unit, message) result *)
  
  (* interface for lsp server *)
  
  (* val typecheck: string -> string * message list *)
  (* val tooltip: string -> int -> string *)
end

module type REPL_FRONTEND = sig
  type ctx
  val empty_ctx: ctx
  val eval_statements: ctx -> string -> (ctx * string, errmsg) result
end


(* Implementation *)

module Repl_frontend (T: THEORY): REPL_FRONTEND = struct
  type ctx = T.ctx
  let empty_ctx = T.empty_ctx
  
  (** itering a list with state ctx *)  
  let eval_statements ctx source =
    let rec eval_stmt ctx echoes xs =
      match xs with
      | [] -> Ok (ctx, String.concat "\n" @@ List.rev @@ List.filter ((<>) "") echoes)
      | x::xs ->
        Result.bind (T.eval T.eval_dir ctx x) 
        (fun (ctx, echo) -> eval_stmt ctx (echo::echoes) xs)
    in Result.bind (T.ast_from_source source) @@ eval_stmt ctx []

end