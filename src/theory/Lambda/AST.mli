type pre_expr = Iden of string | Parened of expr | Abs of string * expr
and expr = Applies of pre_expr list | Simp of expr | Simp1 of expr
val exprP : unit -> expr Parser.parser
type tyexpr = Any
val tyexprP : unit -> tyexpr Parser.parser
(* type dir_arg_t = string *)
type statement = Expr of expr | Let of bool * string * expr | Dir of string * string
val statementP : statement Parser.parser
val statementsP : statement list Parser.parser
