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
  type eval_dir_t = ctx -> string -> string -> eval_result_t
  
  val eval_dir: eval_dir_t
  val eval_dir_deny: eval_dir_t

  val eval: eval_dir_t -> ctx -> statement -> eval_result_t

  val eval_source: eval_dir_t -> ctx -> string -> eval_result_t
  
  (* interface for repl and interpreter *)
  
  (* val import_module: string -> string -> (unit, message) result *)
  
  (* interface for lsp server *)
  
  (* val typecheck: string -> string * message list *)
  (* val tooltip: string -> int -> string *)
end

module type REPL_FRONTEND = sig
  type ctx
  val empty_ctx: ctx
  val eval_source: ctx -> string -> (ctx * string, errmsg) result
end


(* Implementation *)

module Repl_frontend (T: THEORY): REPL_FRONTEND = struct
  type ctx = T.ctx
  let empty_ctx = T.empty_ctx
  
  (** itering a list with state ctx *)  
  let eval_source = T.eval_source T.eval_dir
    
end