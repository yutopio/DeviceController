module Types

type value =
    | Unit
    | Int of int
    | Float of single
and ident =
    | Name of string
and expr =
    | EmptyExpr
