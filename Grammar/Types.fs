module Types

open System
open System.Collections.Generic
open Error

let internalInvokableID = ref 0
let newID () =
    let ret = !internalInvokableID
    internalInvokableID := ret + 1
    ret

type procBodyRaw =
    | R_Time of commandRaw list
    | R_Invoke of string * expr list
and commandRaw =
    | R_Command of string * expr list * (int option * endTime) option
and endTime =
    | To of int
    | For of int
and procBody =
    | Time of command list * int
    | Invoke of invokable * Object list
and command =
    | Command of device * Object list * int * int
and [<AbstractClass>] invokable(name) =
    let _id : int = newID ()
    let mutable _name : string = name

    member this.id
        with get() = _id

    member this.name
        with get() = _name
        and set(value) = _name <- value

    interface IComparable with
        member this.CompareTo(arg:Object) =
            match arg with
            | :? invokable as x -> this.id - x.id
            | _ -> -1

    interface IComparable<invokable> with
        member this.CompareTo(arg:invokable) =
           arg.id - this.id

and procRaw(name, parameters, body) =
    inherit invokable(name)

    let mutable _parameters : string list = parameters
    let mutable _body : procBodyRaw list = body

    override this.ToString() =
        "ProcRaw " + this.name.ToString()

    member this.parameters
        with get() = _parameters
        and set(value) = _parameters <- value

    member this.body
        with get() = _body
        and set(value) = _body <- value

and proc(name) =
    inherit invokable(name)

    (* let mutable _parameters : string list = []*)
    let mutable _body : procBody list = []

    override this.ToString() =
        "Proc " + this.name.ToString()

    (*member this.parameters
        with get() = _parameters
        and set(value) = _parameters <- value*)

    member this.body
        with get() = _body
        and set(value) = _body <- value

and extProc(name, proc, ref) =
    inherit invokable(name)

    let mutable _external : proc = proc
    let mutable _deviceBind : (device * device) list = ref

    override this.ToString() =
        "Ex " + this.external.ToString()

    member this.external
        with get() = _external
        and set(value) = _external <- value

    member this.deviceBind
        with get() = _deviceBind
        and set(value) = _deviceBind <- value

and device(name, deviceType, configuration) =
    inherit invokable(name)

    let mutable _deviceType : string = deviceType
    let mutable _configuration : (string * Object) list = configuration

    override this.ToString() =
        "Dev " + this.name.ToString()

    member this.deviceType
        with get() = _deviceType
        and set(value) = _deviceType <- value

    member this.configuration
        with get() = _configuration
        and set(value) = _configuration <- value

and expr =
    | Const of literal
    | Add of expr * expr
and literal =
    | Value of string
    | String of string
    | Int of int
    | Float of single

let private varNYI () =
    raise (new NotImplementedException("Variable is not supported."))

let EvalLiteral (x:literal) : Object =
    match x with
    | Value _ -> varNYI ()
    | String x -> x :> Object
    | Int x -> x :> Object
    | Float x -> x :> Object

let rec Eval (x:expr) : literal =
    match x with
    | Const x -> x
    | Add(x, y) ->
        let x = Eval x
        let y = Eval y
        match x with
        | String x ->
            match y with
            | String y -> String (x + y)
            | Int y -> String (x + y.ToString())
            | Float y -> String (x + y.ToString())
            | Value _ -> varNYI ()
        | Int x ->
            match y with
            | String y -> String (x.ToString() + y)
            | Int y -> Int (x + y)
            | Float y -> Float ((single)x + y)
            | Value _ -> varNYI ()
        | Float x ->
            match y with
            | String y -> String (x.ToString() + y)
            | Int y -> Float (x + (single)y)
            | Float y -> Float (x + y)
            | Value _ -> varNYI ()
        | Value _ -> varNYI ()
