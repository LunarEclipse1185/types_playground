type pre_expr = Iden of string | Parened of expr | Abs of string * expr
and expr = Applies of pre_expr list | Simp of expr
val exprP : unit -> expr Parser.parser
type tyexpr = Any
val tyexprP : unit -> tyexpr Parser.parser
type dir_arg_t = Expr of expr | Str of string
type statement = Expr of expr | Let of string * expr | Dir of string * dir_arg_t
val statementsP : statement list Parser.parser
