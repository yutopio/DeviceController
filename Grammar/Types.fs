module Types

open System
open System.Collections.Generic

type procBodyRaw =
    | R_Time of commandRaw list
    | R_Invoke of string * expr list
and commandRaw =
    | R_Command of string * expr list * (int option * endTime) option
and endTime =
    | To of int
    | For of int
and procBody =
    | Time of command list
    | Invoke of invokable * expr list
and command =
    | Command of device * expr list * int * int
and [<AbstractClass>] invokable(id) =
    let mutable _id : string = id

    member this.id
        with get() = _id
        and set(value) = _id <- value

and procRaw(id, parameters, body) =
    inherit invokable(id)

    let mutable _parameters : string list = parameters
    let mutable _body : procBodyRaw list = body

    override this.ToString() =
        "ProcRaw " + this.id.ToString()

    member this.parameters
        with get() = _parameters
        and set(value) = _parameters <- value

    member this.body
        with get() = _body
        and set(value) = _body <- value

and proc(id) =
    inherit invokable(id)

    (* let mutable _parameters : string list = []*)
    let mutable _body : procBody list = []

    override this.ToString() =
        "Proc " + this.id.ToString()

    (*member this.parameters
        with get() = _parameters
        and set(value) = _parameters <- value*)

    member this.body
        with get() = _body
        and set(value) = _body <- value

and extProc(id, proc, ref) =
    inherit invokable(id)

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

and device(id, devType, devSpec: (string * literal) list) =
    inherit invokable(id)

    // TODO: What is the default configuration on serial transmission?
    let mutable _portName : string = null
    let mutable _baudRate : int = 9600
    let mutable _parity : int = 0
    let mutable _dataBits : int = 8
    let mutable _stopBits : int = 1

    override this.ToString() =
        "Dev " + this.id.ToString()

    member this.portName
        with get() = _portName
        and set(value) = _portName <- value

    member this.baudRate
        with get() = _baudRate
        and set(value) = _baudRate <- value

    member this.parity
        with get() = _parity
        and set(value) = _parity <- value

    member this.dataBits
        with get() = _dataBits
        and set(value) = _dataBits <- value

    member this.stopBits
        with get() = _stopBits
        and set(value) = _stopBits <- value
and expr =
    | Const of literal
    | Add of expr * expr
and literal =
    | Value of string
    | String of string
    | Int of int
    | Float of single
