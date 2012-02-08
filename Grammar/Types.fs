module Types

type procBody =
    | Time of command list
    | Proc of ident * expr list
and command =
    | Command of ident * expr list * (int option * endTime) option
and endTime =
    | To of int
    | For of int
and ident = int * string
and proc =
    | ProcDef of ident * ident list * procBody list
and expr =
    | Const of literal
    | Add of expr * expr
and literal =
    | Value of ident
    | String of string
    | Int of int
    | Float of single
