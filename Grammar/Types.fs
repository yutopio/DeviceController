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
    | Time of command list
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

and device(name:string) =
    inherit invokable(name)

    override this.ToString() =
        "Dev " + this.name.ToString()

    static member create(name, devType, devSpec) =
        match devType with
        | null -> new device(name)
        | "serial" -> new serial(name, devSpec) :> device
        | _ -> unknownDevType devType

and serial(name:string, devSpec: (string * literal) list) =
    inherit device(name)

    // TODO: What is the default configuration on serial transmission?
    let mutable _portName : string = null
    let mutable _baudRate : int = 9600
    let mutable _parity : int = 0
    let mutable _dataBits : int = 8
    let mutable _stopBits : int = 1

    override this.ToString() =
        "Dev " + this.name.ToString()

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
