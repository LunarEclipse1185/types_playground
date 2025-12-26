(* Untyped Lambda type system. *)

open Core

module Syntax = struct
  include AST

  (* type expr = AST.expr
  type tyexpr = AST.tyexpr
  type statement = AST.statement *)

  let assert_exhaust = fun r -> Result.bind r
    (fun (x, rest) -> if rest = "" then Ok x else
      Error {msg = "Parser stopped early. Rest: \"" ^ rest ^ "\""; start = 0; stop = 0;})

  let ast_from_source src =
    assert_exhaust @@ Parser.run statementsP @@ String.trim src
end

module Semantics = struct
  include Syntax
  
  type tm =
  | Var of int (** de Bruijn index starting from 1 *)
  | Abs of tm
  | Apply of tm * tm
  
  type ty =
  | Any

  let rec eq l r = match (l, r) with
  | (Var l, Var r) -> l = r
  | (Abs l, Abs r) -> eq l r
  | (Apply (l1, l2), Apply (r1, r2)) -> eq l1 r1 && eq l2 r2
  | _ -> false
  
  let (* rec *) ty_eq l r = match (l, r) with
  | (Any, Any) -> true
  
  (* maybe abstract out [mapi_var] function *)
  let[@tail_mod_cons] rec lift_free_var x n depth =
  match x with
  | Var index as var -> if index > depth then Var (index + n) else var
  | Abs x -> Abs (lift_free_var x n @@ depth+1)
  | Apply (x, x') -> Apply (lift_free_var x n depth, (lift_free_var[@tailcall]) x' n depth)
  
  let rec not_occurred index = function
  | Var ind -> ind <> index
  | Abs x -> not_occurred (index+1) x
  | Apply (x, x') -> not_occurred index x && not_occurred index x'
  
  (** beta reduce helper. replaces all references of de Bruijn index [depth] in [x] by [y] *)
  let[@tail_mod_cons] rec substitute x y depth = 
    match x with
    | Var index ->
      if index = depth then lift_free_var y (index-1) 0 else
      if index > depth then Var (index - 1) else Var index
      (* beta: indexes decremented here *)
    | Abs x -> Abs (substitute x y @@ depth+1)
    | Apply (x, x') -> Apply (substitute x y depth, (substitute[@tailcall]) x' y depth)
    
  let [@tail_mod_cons] rec beta_reduce = function
  | Var _ as var -> var (* keep same *)
  | Abs x -> Abs (beta_reduce x)
  | Apply (Abs x, x') -> beta_reduce @@ substitute x x' 1
  (* 1 because we are already destructuring 1 layer here *)
  | Apply (x, x') -> (* here x will not ever be Abs, but can reduce to Abs *)
    let y = beta_reduce x in
    let y' = beta_reduce x' in
    match y with (* to prevent dead loop, and avoid early stops *)
    | Abs _ -> beta_reduce @@ Apply (y, y')
    | _ -> Apply(y, y')
    
  let[@tail_mod_cons] rec eta_reduce = function
  | Abs (Apply (x, Var 1)) ->
    if not_occurred 1 x
    then eta_reduce (lift_free_var x (-1) 0)
    else Abs (Apply (eta_reduce x, Var 1))
  | Abs x -> Abs (eta_reduce x)
  | Apply (x, x') -> Apply (eta_reduce x, (eta_reduce[@tailcall]) x')
  | x -> x
  
  (** Assuming [x] is normalizable, performs normal-order algorithm from
  top to down resulting in the beta-eta normal form. *)
  let normalize x = eta_reduce @@ beta_reduce x

  (* perform 1 step of reduction in the same order as used above,
  except that parallel reductions in Apply branches get reduced at the same time.
  it is better to use the above version if partial results are not to be used. *)
  let[@tail_mod_cons] normalize_step x =
    (* bool: didChange *)
    let[@tail_mod_cons] rec beta_step = function
    | Var _ as var -> (var, false)
    | Abs x -> let (x', did_change) = beta_step x in (Abs (x'), did_change)
    | Apply (Abs x, x') -> (substitute x x' 1, true)
    (* 1 because we are already destructuring 1 layer here *)
    | Apply (x, x') ->
      let (y, did_change) = beta_step x in
      let (y', did_change') = beta_step x' in
      (Apply(y, y'), did_change || did_change')
    in

    let[@tail_mod_cons] rec eta_step = function
    | Abs (Apply (x, Var 1)) ->
      if not_occurred 1 x
      then (lift_free_var x (-1) 0, true)
      else let (y, did_change) = eta_step x in
        (Abs (Apply (y, Var 1)), did_change)
    | Apply (x, x') ->
      let (y, did_change) = eta_step x in
      let (y', did_change') = eta_step x' in
      (Apply (y, y'), did_change || did_change')
    | x -> (x, false)
    in

    match beta_step x with
    | (x, false) -> eta_step x
    | res -> res

  type ctx = { bindings: (string * tm) list; defaults: string list }
  let empty_ctx: ctx = { bindings = []; defaults = [] }

  let ctx_bind ctx iden value default = {
    bindings = (iden, value)::(List.filter (fun x -> fst x <> iden) ctx.bindings);
    defaults =
      let shadowed = List.exists ((=) iden) ctx.defaults in
      if default && shadowed || not default && not shadowed then
        ctx.defaults
      else if default && not shadowed then
        iden::ctx.defaults
      else (* !default && shadowed *)
        List.filter ((<>) iden) ctx.defaults
  }

  let parse_tm (ctx: ctx) x: (tm, errmsg) result =
    let exception Unbound of string in

    let empty_locctx: string list = [] in

    let search_local locctx id =
      List.find_index ((=) id) locctx
    in
    let search_global { bindings; _ } id =
      try List.assoc id bindings with Not_found -> raise (Unbound id)
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

    (* TODO: It is ugly to put core logic in the parsing of expr *)
    and[@tail_mod_cons] parse_expr locctx = function
    | Applies xs -> parse_applies locctx xs
    | Simp x -> normalize (parse_expr locctx x)
    | Simp1 x -> fst @@ normalize_step (parse_expr locctx x)

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
    | y::ys -> Arrow (parse_pre y, (parse_applies[@tailcall]) ys)
    (* the two cases are actually equally possible *)
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
  | Abs tm -> "\\ " ^ string_of_tm tm
  | Apply (Abs _ as l, r) ->
    "(" ^ string_of_tm l ^ ") " ^ string_of_tm r
  | Apply (l, (Apply _ as r)) ->
    string_of_tm l ^ " (" ^ string_of_tm r ^ ")"
  | Apply (l, r) ->
    string_of_tm l ^ " " ^ string_of_tm r

  let string_of_tm_pretty ctx tm =
    (* starts from `a` and basically does a base-26 numbering *)
    let increment_name names =
      let rec increment_name old =
        let old = String.to_bytes old in
        let len = Bytes.length old in
        let new_name = try for i = len-1 to 0 do
          let char = Bytes.get old i in
          if char < 'z'
          then (Bytes.set old i (Char.chr @@ Char.code char + 1); raise Exit)
          else Bytes.set old i 'a'
        done; String.make (len+1) 'a' with
        | Exit -> String.of_bytes old
        in
        if (List.exists (fun (x, _) -> x = new_name) ctx.bindings)
        then increment_name new_name (* aka while loop *)
        else new_name
      in
      match names with
      | [] ->
        let initial = "a" in
        if (List.exists (fun (x, _) -> x = initial) ctx.bindings)
        then increment_name initial (* aka while loop *)
        else initial
      | name::_ -> increment_name name
    in
    let rec string_of_tm_pretty names = function
    | Var index -> List.nth names (index-1)
    | Abs tm ->
      let name = increment_name names in
      "\\" ^ name ^ ". " ^ string_of_tm_pretty (name::names) tm
    | Apply (Abs _ as l, r) ->
      "(" ^ string_of_tm_pretty names l ^ ") " ^ string_of_tm_pretty names r
    | Apply (l, (Apply _ as r)) ->
      string_of_tm_pretty names l ^ " (" ^ string_of_tm_pretty names r ^ ")"
    | Apply (l, r) ->
      string_of_tm_pretty names l ^ " " ^ string_of_tm_pretty names r
    in
    string_of_tm_pretty [] tm
end

include Semantics

(* Silly Me, there is NO such thing as beta reduction on a untypedLambda type *)
(* let beta_reduct_type y1 y2: ty =
  _ *)

(* actually wrong , did not 'infer' the arguments' types*)
(* most generally, infer produces infinite structures and is hence not implementable *)
(* let infer _ x: (ty, errmsg) result =
  let rec infer x: ty = match x with
  | Var _ -> Any
  | Abs x -> Arrow (Any, infer x)
  | Apply _ -> Any
  in Ok (infer x) *)
let infer _ _ = Ok Any


type eval_result_t = (ctx * (tm option) * string, errmsg) result

let (let*) = Result.bind

(** echo string specialized for repl environment *)
let repl_echo_tm ctx id x =
  Option.value id ~default:"-" ^
  " = " ^
  string_of_tm_pretty ctx x

(* let-binding an existing name overrides the old one *)
let eval eval_dir ctx: statement -> eval_result_t = function
| Expr x ->
    let* tm = parse_tm ctx x in
    (* let* ty = infer ctx tm in *)
    Ok (ctx, Some tm, repl_echo_tm ctx None tm)
| Let (default, id, x) ->
    let* tm = parse_tm ctx x in
    let ctx' = ctx_bind ctx id tm false in (* to avoid the name in trace, while not showing id=id *)
    Ok (ctx_bind ctx id tm default, Some tm,
      (if default then "default " else "") ^
      repl_echo_tm ctx' (Some id) tm)
| Dir (dir, x) -> eval_dir ctx dir x

let eval_trace eval_dir ctx stmt max_depth: eval_result_t = 
  let rec trace ctx tm echoes depth =
    let (tm, did_change) = normalize_step tm in
    if did_change then trace ctx tm (string_of_tm_pretty ctx tm::echoes) (depth-1)
    else (tm, String.concat "\n" (List.rev @@ List.filter ((<>) "") echoes))
  in
  match stmt with
  | Expr x ->
    let* tm = parse_tm ctx x in
    let (tm, echo) = trace ctx tm [] max_depth in
    Ok (ctx, Some tm, echo ^ repl_echo_tm ctx None tm)
  | Let (default, id, x) ->
    let* tm = parse_tm ctx x in
    let ctx' = ctx_bind ctx id tm false in (* to avoid the name in trace, while not showing id=id *)
    let (tm, echo) = trace ctx' tm [] max_depth in
    Ok (ctx_bind ctx id tm default, Some tm, echo ^ repl_echo_tm ctx (Some id) tm)
  | Dir (dir, x) -> eval_dir ctx dir x

(* different dir eval options for combining with eval *)

(* type dir_arg_t = Expr of expr | Str of string | None *)
type eval_dir_t = ctx -> string -> string -> eval_result_t

let eval_dir_deny _ dir _: eval_result_t =
  Error {msg = "Unexpected REPL directive " ^ dir; start = 0; stop = 0}

let eval_source (eval_dir: eval_dir_t) ctx source: eval_result_t =
  let rec eval_stmts ctx (value: tm option) echoes = function
    | [] -> Ok (ctx, value,
      String.concat "\n" (List.rev @@ List.filter ((<>) "") echoes))
    | x::xs ->
      Result.bind (eval eval_dir ctx x) 
      (fun (ctx, value, echo) -> eval_stmts ctx value (echo::echoes) xs)
  in Result.bind (ast_from_source source) @@ eval_stmts ctx None []


let eval_dir (ctx: ctx) dir (arg: string): eval_result_t =
  let read_file path =
    try Ok (In_channel.input_all @@ open_in path) with
    Sys_error e -> Error {msg = e; start = 0; stop = 0}
  in

  let rec print_context echoes bindings defaults = match bindings with
  | [] -> String.concat "\n" echoes ^ "\n" (* the order is just right *)
  | (id, x)::tail -> print_context (
      ((if List.exists ((=) id) defaults then "default " else "") ^
      repl_echo_tm ctx (Some id) x) :: echoes
    ) tail defaults
  in
  match dir with
  | "q" | "quit" -> exit 0
  | "c" | "ctx" | "context" -> Ok (ctx, None, print_context [] ctx.bindings ctx.defaults)
  | "l" | "load" ->
      let* source = read_file @@ String.trim arg in
      eval_source eval_dir_deny ctx source
  | "trace" ->
    (* TODO: parse an optional depth on-site, e.g. :trace 20 <term> *)
    let* stmt = assert_exhaust @@ Parser.run statementP @@ String.trim arg in
    eval_trace eval_dir_deny ctx stmt 20
  | "h" | "help" ->
    let help =
      "Language discription of Untyped Lambda Calculus:\n" ^
      "Type a term to inspect it;\n" ^
      "Use `let name = term` to create bindings;\n" ^
      "Use `let default name = term` to auto substitute `name` for `term` in all echoes.\n" ^
      "`simp` and `simp1` are magic functions for full normalizing and 1 step of normalizing.\n" ^
      "\n" ^
      "REPL Directives:\n" ^
      ":q | quit\t\tQuit REPL\n" ^
      ":h | help\t\tPrint this message\n" ^
      ":c | ctx | context\tLog the current context\n" ^
      ":l | load <path>\tLoad statements from a file with extension `.l'\n" ^
      ":trace <term>\t\tLog the simplification process of a term until termination\n"
    in Ok(ctx, None, help)
  | "verbose" (* not implemented *)
  | "quiet"
  | _ -> eval_dir_deny ctx dir arg
