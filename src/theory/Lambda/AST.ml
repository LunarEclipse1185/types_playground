(** Implementation of Parser Combinator in a Eager-Evaluated fasion *)
(** Without a lexing stage *)

open Parser

(** tweaked to avoid left recursion, not the same as semantic tree *)
(* type pre_expr =
| Iden of string (* local variable or let-bound name, local overrides global *)
| Abs of string * expr (* \x. M *)
| Apply of expr * expr (* (a b) *)
and expr =
| Applies of pre_expr list *)

let is_iden_char c = is_alnum c || c = '_'


(* 3 kinds of open operations: simp, \x.M and application; 1 kind of closed op: (expr)
where Apply is left assoc; Simp and Abs no need of associativity;
Precedence: Apply before Abs before Simp, so that `simp M \x. N P` is equivalent to `simp (M \x. (N P)`.
The parser calling order is reversed: preventing `(simp M) (\x. N) P` and early stopping on `M simp N`. More generally, outermost least-referenced right-open constructs need to be matched first. Though the wording needs consideration. *)

(* in the right-recursive definition expr made of 1 identifier is classified into application chain, which in turn is viewed as the highest level structure *)

type pre_expr =
| Iden of string (* 0-9a-zA-Z_ *)
| Parened of expr (* (M) *)
| Abs of string * expr (* \x. M *)
and expr =
| Applies of pre_expr list (* M N P *)
| Simp of expr
| Simp1 of expr

let keywords = ["let"; "simp"; "simp1"; "default"]
let idenP (): string parser =
  spanP is_iden_char |> only_if (fun id -> List.exists ((=) id) keywords = false)
  
let rec parenedP (): pre_expr parser =
  (fun x -> Parened x) <$>
  ws *> charP '(' *>
  ws *> exprP () <*
  ws *> charP ')'
and absP (): pre_expr parser =
  (fun x y -> Abs (x, y)) <$>
  ws *> charP '\\' *>
  ws *> idenP () <*>
  ws *> charP '.' *>
  ws *> exprP ()
(* and applyP (): pre_expr parser =
  (fun x y -> Apply (x, y)) <$>
  ws *> charP '(' *>
  ws *> exprP () <*>
  ws *> exprP () <*
  ws <* charP ')' *)
and preP (): pre_expr parser =
  (fun x -> Iden x) <$> idenP () <|> parenedP () <|> absP ()
and appliesP (): expr parser =
  (fun x -> Applies x) <$> some (ws *> preP ())
and simpP (): expr parser =
  (fun x -> Simp x) <$> ws *> tokenP is_iden_char "simp" *> exprP ()
and simp1P (): expr parser =
  (fun x -> Simp1 x) <$> ws *> tokenP is_iden_char "simp1" *> exprP ()
and exprP () = defer @@ fun () -> simpP () <|> simp1P () <|> appliesP ()
(* and exprP (): expr parser = appliesP () *)


type tyexpr = Any
let tyexprP () =
  Fun.const Any <$>
  ws *> charP '_'
(* 
type pre_tyexpr =
| TyParened of tyexpr (* (M) *)
| Any (* _ *)
and tyexpr =
| TyApplies of pre_tyexpr list (* M -> N -> P *)

let rec tyAnyP (): pre_tyexpr parser =
  Fun.const Any <$>
  ws *> charP '_'
(* let rec tyArrowP (): pre_tyexpr parser = (* simply *)
  (fun x y -> Arrow (x, y)) <$>
  ws *> charP '(' *>
  ws *> tyexprP () <*>
  ws *> stringP "->" *>
  ws *> tyexprP () <*
  ws <* charP ')' *)
and tyParenedP (): pre_tyexpr parser =
  (fun x -> TyParened x) <$>
  ws *> charP '(' *>
  ws *> tyexprP () <*
  ws *> charP ')'
and tyPreP (): pre_tyexpr parser =
  tyParenedP () <|> tyAnyP ()
and appliesP (): tyexpr parser =
  (fun x -> TyApplies x) <$> (
    List.cons <$>
    tyPreP () <*>
    many (ws *> stringP "->" *> tyPreP ()))
and tyexprP () = defer @@ fun () -> appliesP () *)

type statement =
| Expr of expr (* check type and value *)
| Let of bool * string * expr (* let a = *)
| Dir of string * string (* REPL direction *)
(* and dir_arg_t = string *)

let letP: statement parser =
  (fun x y z -> Let (x, y, z)) <$>
  ws *> stringP "let" *>
  ws *> (asBool @@ stringP "default") <*>
  ws *> idenP () <*>
  ws *> charP '=' *>
  ws *> exprP ()


let dirP: statement parser =
  (* let exprArgP = ws *> ((fun x -> (Expr x: dir_arg_t)) <$> exprP ()) in *)
  let argP = optionalSpanP (fun c -> c <> '\n') in
  (fun x y -> Dir (x, y)) <$>
  ws *> charP ':' *>
  (* no ws *) spanP is_iden_char <*> argP
  (* (exprArgP <|> fallbackP) *)
  
  
let statementP: statement parser =
  (fun x -> Expr x) <$> exprP ()
  <|> letP <|> dirP
  <* ws <* optional @@ charP ';'

let statementsP: statement list parser =
  many statementP