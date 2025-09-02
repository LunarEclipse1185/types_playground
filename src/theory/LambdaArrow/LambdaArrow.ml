(* Lambda-> type system, also known as the Simply Typed Lambda Calculus. *)
(* Eager evaluated. *)

(*TODO: change this module name to Lambda and make repl default to lambda *)
(*TODO: file loading and echoing by directive load (change type to any string and parse them in directive eval), ext=.l *)
(*TODO: directive echo strength; type system lifting *)
open Core

module Syntax = struct
  include AST

  (* type expr = AST.expr
  type tyexpr = AST.tyexpr
  type statement = AST.statement *)

  let ast_from_source src =
    let finalize = fun r -> Result.bind r
      (fun (x, rest) -> if rest = "" then Ok x else
        Error {msg = "Parser stopped early. Rest: \"" ^ rest ^ "\""; start = 0; stop = 0;})
    in finalize @@ Parser.run statementsP @@ String.trim src
end

module Semantics = struct
  include Syntax
  
  type tm =
  | Var of int (** de Bruijn index starting from 1 *)
  | Abs of tm
  | Apply of tm * tm
  
  type ty =
  | Any
  (* | Arrow of ty * ty *)
  let (* rec *) ty_eq l r = match (l, r) with
  | (Any, Any) -> true
  (* | (Arrow (ll, lr), Arrow (rl, rr)) -> ty_eq ll rl && ty_eq lr rr
  | _ -> false *)

  (** Assuming [x] is normalizable, performs beta reduction from left to right. *)
  let[@tail_mod_cons] rec normalize x =
    (* we can abstract out [mapi_var] function *)
    let[@tail_mod_cons] rec shift_free_index x n depth =
      match x with
      | Var index as var -> if index > depth then Var (index + n) else var
      | Abs x -> Abs (shift_free_index x n @@ depth+1)
      | Apply (x, x') -> Apply (shift_free_index x n depth, (shift_free_index[@tailcall]) x' n depth)
    in

    (* replace all references of de Bruijn index [depth] in [x] by [y] *)
    let[@tail_mod_cons] rec reduct x y depth = 
      match x with
      | Var index as var -> if index = depth then shift_free_index y (index-1) 0 else var
      | Abs x -> Abs (reduct x y @@ depth+1)
      | Apply (x, x') -> Apply (reduct x y depth, (reduct[@tailcall]) x' y depth)
      (* we perform 1 reduction at once otherwise we'll need to store multiple terms and the index will change. depth only increases when we go 1 Abs deeper. *)
    in
    
    match x with
    | Var _ as var -> var (* keep same *)
    | Abs x -> Abs (normalize x)
    | Apply (x, x') ->
      match normalize x with
      | Abs x -> reduct x x' 1 (* we are already destructing 1 layer here *)
      | _ as x -> Apply (x, x') (* not beta-reducible *)

  type ctx = (string * tm) list
  let empty_ctx: ctx = []

  let parse_tm (ctx: ctx) x: (tm, errmsg) result =
    let exception Unbound of string in

    let empty_locctx: string list = [] in

    let search_local locctx id =
      List.find_index ((=) id) locctx
    in
    let search_global ctx id =
      try List.assoc id ctx with Not_found -> raise (Unbound id)
    in
 
    let[@tail_mod_cons] rec parse_applies locctx =
      let[@tail_mod_cons] rec finalize = function
        | []-> failwith "AST parser yields an empty apply-list"
        | x::[] -> x
        | x::xs -> Apply (finalize xs, x)
      in
      let[@tail_mod_cons] rec parse_applies locctx acc = function
      | x::xs -> parse_applies locctx (parse_pre locctx x :: acc) xs
      | [] -> finalize acc
      in parse_applies locctx []

    and[@tail_mod_cons] parse_expr locctx = function
    | Applies xs -> parse_applies locctx xs
    | Simp x -> normalize (parse_expr locctx x)

    and[@tail_mod_cons] parse_pre locctx = function
    | Parened x -> parse_expr locctx x
    | Abs (id, x) -> Abs (parse_expr (id::locctx) x) (* push stack *)
    | Iden id -> match search_local locctx id with
      | Some index -> Var (index + 1) (* index starting from 1 *)
      | None -> (search_global[@tailcall false]) ctx id
    
    in try Ok (parse_expr empty_locctx x)
    with Unbound id -> Error {msg = "Unbound identifier " ^ id; start = 0; stop = 0}
  
  (* simplified using the structure of tyexpr *)
  (* let parse_ty _ (TyApplies ys: tyexpr): (ty, errmsg) result =
    let[@tail_mod_cons] rec parse_applies = function
    | [] -> failwith "AST type parser yields an empty apply-list"
    | y::[] -> parse_pre y
    | y::ys -> Arrow (parse_pre y, (parse_applies[@tailcall]) ys) (* the two cases are actually equally possible *)
    and[@tail_mod_cons] parse_pre: pre_tyexpr -> ty = function
    | Any -> Any
    | TyParened TyApplies ys -> parse_applies ys
    in Ok (parse_applies ys) *)
  
  let parse_ty _ (Any: tyexpr) = Ok Any

  let (* rec *) string_of_ty ty _ = match ty with
  | Any -> "_"
  (* | Arrow (l, r) ->
    let res = string_of_ty l true ^
    " -> " ^
    string_of_ty r false
    in if left then "(" ^ res ^ ")" else res *)

  let string_of_ty ty =
    string_of_ty ty false

  let rec string_of_tm = function
    | Var index -> string_of_int @@ index
    | Abs trm -> "\\ " ^ string_of_tm trm
    | Apply (Abs _ as l, r) ->
      "(" ^ string_of_tm l ^ ") " ^ string_of_tm r
    | Apply (l, (Apply _ as r)) ->
      string_of_tm l ^ " (" ^ string_of_tm r ^ ")"
    | Apply (l, r) ->
      string_of_tm l ^ " " ^ string_of_tm r

end

include Semantics

(* if we do the same on term, the eager recursive eval will force reduction from left to right *)
(* Silly Me, there is NO such thing as beta reduction on a type *)
(* let beta_reduct_type y1 y2: ty =
  _ *)

(* actually wrong , did not 'infer' the arguments' types*)
(* more generally, infer produces infinite structures and is hence not implementable *)
(* let infer _ x: (ty, errmsg) result =
  let rec infer x: ty = match x with
  | Var _ -> Any
  | Abs x -> Arrow (Any, infer x)
  | Apply _ -> Any
  in Ok (infer x) *)
let infer _ _ = Ok Any


type eval_result_t = (ctx * string, errmsg) result

let (let*) = Result.bind

let echo_tm id x = Option.value id ~default:"-" ^ " = " ^ string_of_tm x

(* let-binding an existing name overrides the old one but still grows the context *)
let eval eval_dir ctx: statement -> eval_result_t = function
| Expr x ->
    let* tm = parse_tm ctx x in
    (* let* ty = infer ctx tm in *)
    Ok (ctx, echo_tm None tm)
| Let (id, x) ->
    let* tm = parse_tm ctx x in
    Ok ((id, tm)::(List.filter (fun x -> fst x <> id) ctx), echo_tm (Some id) tm)
| Dir (dir, x) -> eval_dir ctx dir x

(* different dir eval options for combining with eval *)

let eval_dir_deny _ dir _: eval_result_t =
  Error {msg = "Unexpected REPL directive " ^ dir; start = 0; stop = 0}

let eval_dir (ctx: ctx) dir x: eval_result_t =
  let rec print_context echoes = function
  | [] -> String.concat "\n" echoes (* the order is just right *)
  | (id, x)::tail -> print_context (echo_tm (Some id) x :: echoes) tail
  in match dir with
  | "q" | "quit" -> exit 0
  | "c" | "ctx" | "context" -> Ok (ctx, print_context [] ctx)
  | "simp" -> (match x with
    | Some x ->
      let* x = parse_tm ctx x in
      Ok (ctx, echo_tm None @@ normalize x)
    | None -> Error {msg = "Directive simp expects 1 expr but got 0"; start = 0; stop = 0})
  | "h" | "help" ->
    let help =
      "REPL Directives:\n" ^
      ":q | quit\t\tQuit REPL\n" ^
      ":h | help\t\tPrint this message\n" ^
      ":c | ctx | context\tLog the current context\n" ^
      ":simp <expr>\t\tBeta-normalize the expression"
    in Ok(ctx, help)
  | "verbose" (* not implemented *)
  | "default"
  | "quiet"
  | _ -> eval_dir_deny ctx dir x
