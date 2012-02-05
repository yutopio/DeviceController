module Types

type procBody =
    | Time of command list
    | Proc of ident * expr list
and command =
    | Command of ident * expr list * (single option * endTime) option
and endTime =
    | To of single
    | For of single
and ident = string
and proc =
    | ProcDef of ident * ident list * procBody list
and expr =
    | Const of literal
    | Add of expr * expr
and literal =
    | Value of string
    | String of string
    | Float of single
