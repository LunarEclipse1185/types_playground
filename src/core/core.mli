(**
The modular syntax system for multiple type theory interfaces.
SYNTAX: As of now, a piece of source code consists of multiple statements, while providing two expression types for further utilities. A piece of source code is converted here to a list of AST's, whose type matching and identifier resolution are deferred to the semantic stage.
SEMANTICS: [eval] is the public interface for code execution. parse_tm/ty: One expression at a time, context information is added and corresponding checks and analyses are performed. Different languages handle [eval] very differently. Other functions are made public for lsp functionality and for standardizing purpose.

Error reporting code position will need source map functionality, which should(?) be built into all the syntax and semantics types. Easy implementations report 0:0 on error.
*)

type errmsg = { msg: string; start: int; stop: int; }

module type SYNTAX = sig
  type expr
  type tyexpr
  type statement
  (** abstract syntax tree for both file and interactive source code,
  which is  *)

  val ast_from_source: string -> (statement list, errmsg) result
  (** the same parser is used for file and interactive parsing. *)
end

(** syntax for a type theory *)
module type SEMANTICS = sig
  include SYNTAX

  type tm
  (** a semantically (fully) legal term *)

  type ty
  (** type of a term *)

  type ctx
  val empty_ctx: ctx

  val parse_tm: ctx -> expr -> (tm, errmsg) result
  (** parse from ast a term. *)

  val parse_ty: ctx -> tyexpr -> (ty, errmsg) result
  (** parse from ast type annotation a type. *)

  val eq: tm -> tm -> bool
  (** decide if two terms are equal, if the system has normal forms. *)

  val ty_eq: ty -> ty -> bool
  (** decide if two types are equal in the type system. *)

  val string_of_ty: ty -> string
  (** pretty printing a type *)

  val string_of_tm: tm -> string
  (** pretty printing a term *)
end

(** internal details for a language *)
module type THEORY = sig
  include SEMANTICS


  val infer: ctx -> tm -> (ty, errmsg) result
  (** infer the type of a given term with context *)

  type eval_result_t = (ctx * tm option * string, errmsg) result
  type eval_dir_t = ctx -> string -> string -> eval_result_t

  val eval_dir: eval_dir_t
  val eval_dir_deny: eval_dir_t

  val eval: eval_dir_t -> ctx -> statement -> eval_result_t

  val eval_source: eval_dir_t -> ctx -> string -> eval_result_t
  (** evaluate a piece of code, executing all its side-effects, potentially modifying the context, and providing an echo *)


  (* interface for repl and interpreter *)

  (* val import_module: string -> string -> (unit, message) result *)
  (** parse a module and import the symbols into the current context. the first argument is module name, and the second argument is source code. *)

  (* val eval: string -> (string, message) result *)
  (** evaluate a single instruction and return the echo string. *)

  (* interface for lsp server *)

  (* val typecheck: string -> string * message list *)
  (** check the grammar and type correctness of a piece of source code and  *)

  (* val tooltip: string -> int -> string *)
end

module type REPL_FRONTEND = sig
  type ctx
  val empty_ctx: ctx

  val eval_source: ctx -> string -> (ctx * string, errmsg) result
  (** take a piece of source code, evaluate them and return the echo string or error message *)

  (* val import_module: ctx -> string -> string -> ctx * (string, message) result *)
  (** take a module name and source code and load all the symbols *)
end

module Repl_frontend: THEORY -> REPL_FRONTEND