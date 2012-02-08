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
and invokable(id) =
    let mutable _defined : bool = false
    let mutable _id : ident = id

    member this.defined
        with get() = _defined
        and set(value) = _defined <- value

    member this.id
        with get() = _id
        and set(value) = _id <- value
and proc(id, parameters, body) =
    inherit invokable(id)

    let mutable _parameters : ident list = parameters
    let mutable _body : procBody list = body
    let mutable _origin : (int * ident) option = None
    let mutable _deviceBind : (device * device) list = []

    member this.parameters
        with get() = _parameters
        and set(value) = _parameters <- value

    member this.body
        with get() = _body
        and set(value) = _body <- value

    member this.origin
        with get() = _origin
        and set(value) = _origin <- value

    member this.deviceBind
        with get() = _deviceBind
        and set(value) = _deviceBind <- value
and device(id) =
    inherit invokable(id)

    let mutable _portName : string = null
    let mutable _baudRate : int = 9600
    let mutable _parity : int = 0
    let mutable _dataBits : int = 0
    let mutable _stopBits : int = 0

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
    | Value of ident
    | String of string
    | Int of int
    | Float of single
