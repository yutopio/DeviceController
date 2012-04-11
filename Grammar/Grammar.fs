module Grammar

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open Types

let mutable loaded = new List<FileInfo>();
let mutable programInfo = new List<Types.invokable list>();
let mutable loadStack = new Stack<int>();

let Reset () =
    loaded <- new List<FileInfo>()
    programInfo <- new List<Types.invokable list>()
    loadStack <- new Stack<int>()

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

type CommandComparer() =
    inherit Comparer<Command>()

    override obj.Compare(x:Command, y:Command) =
        match x with (d1, _, t1, _) ->
        match y with (d2, _, t2, _) ->
        let t = t1 - t2
        if t <> 0 then
            t
        else
            let (i1, _) = d1.id
            let (i2, _) = d2.id
            i1 - i2

let ValidateTimeline devTable commands =
    // First add all parsed commands with semantics.
    let set = new SortedSet<Command>(new CommandComparer())
    let rec Inner commands time =
        match commands with
        | [] -> ()
        | Command(dev, arg, timeSpec) :: rest ->
            let dev = ChooseDevice devTable dev
            let arg = List.map (Eval >> EvalLiteral) arg
            match timeSpec with
            | None ->
                let invocation = (dev, arg.ToArray(), time, time)
                let _ = set.Add(invocation)
                Inner rest time
            | Some(t1, t2) ->
                let t1 = match t1 with Some t1 -> t1 | None -> time
                let t2 = match t2 with For t2 -> t1 + t2 | To t2 -> t2
                let invocation = (dev, arg.ToArray(), t1, t2)
                let _ = set.Add(invocation)
                Inner rest t2
    Inner commands 0

    // Verify that the timeline is correctly aligned by time.
    let blockedTime = new Dictionary<device, int>()
    let rec Inner commands =
        match commands with
        | [] -> ()
        | (dev, _, t1, t2) :: rest ->
            if not (blockedTime.ContainsKey(dev)) then
                blockedTime.Add(dev, t2)
                Inner rest
            else if t1 > t2 then
                raise (new ArgumentOutOfRangeException(String.Format(
                    "Invalid time specification: {0}ms - {1}ms", t1, t2)))
            else if t1 < blockedTime.[dev] then
                // TODO: Not friendly exception message (Device name?)
                raise (new ArgumentOutOfRangeException(String.Format(
                    "Overlapping command specification for device {0} at {1}ms - {2}ms",
                    dev.portName, t1, t2)))
            else
                blockedTime.[dev] <- t2
                Inner rest
    Inner (set.Aggregate([], (fun x y -> x @ [y])))

    // Return converted timeline
    (blockedTime.Keys.ToArray(), set.ToArray(),
        if blockedTime.Count = 0 then 0 else blockedTime.Max(fun (x:KeyValuePair<device, int>) -> x.Value))

let ValidateProc devTable (procs:Dictionary<string, invokable>) (proc:proc) =
    let devices = new List<device>()
    let rec Internal procBody ret =
        match procBody with
        | [] -> ret
        | elem :: rest ->
            let elem =
                match elem with
                | Time commands ->
                    let timeline = ValidateTimeline devTable commands
                    devices.AddRange(let (d, _, _) = timeline in d)
                    T(timeline)
                | Proc(ident, args) ->
                    let (_, name) = ident
                    let args = List.map (Eval >> EvalLiteral) args
                    I(( if procs.ContainsKey(name) then procs.[name]
                        else
                            let d = (ChooseDevice devTable ident)
                            devices.Add(d)
                            d :> invokable), args)
            Internal rest (ret @ [elem])
    proc.Body <- Internal proc.body []
    proc.Devices <- devices.Distinct().ToArray()

let rec Parse (file:FileInfo) : invokable list =
    let stream = new FileStream(file.FullName, FileMode.Open, FileAccess.Read)
    let reader = new StreamReader(stream)

    // Try to parse the file.
    let types, load =
        Parser.compilationUnit Lexer.token
            (LexBuffer<char>.FromTextReader(reader))

    // Close the file.
    reader.Close()

    // We should guarantee free of duplicate identity on different invokables.
    let names = List.map (fun (x : invokable) -> x.id) types
    let checkDuplicateName list =
        match list with
        | [] -> ()
        | elem :: rest ->
            if List.exists ((=) elem) rest then
                raise (new ApplicationException("Duplicate name: " + elem))
    checkDuplicateName names

    // Load external files.
    let extProcs = List.fold(fun extProcs (fileName, subst) ->
        let programInfo = Load fileName
        let mapping = List.map (fun (src, dst) ->
            try
                (List.find (fun (x : invokable) -> x.id = src) programInfo), dst
            with
                | :? KeyNotFoundException ->
                    raise (new ApplicationException(String.Format(
                        "No such procedure or device named {0} defined in {1}.",
                        fileName, src)))) subst

        let (deviceSubst, procSubst) = List.fold (fun ret (src:invokable, dst) ->
            match src with
            | :? device as src ->
                match List.find (fun (x : invokable) -> x.id = dst) types with
                | :? device as dst ->
                    let a, b = ret in (src, dst) :: a, b
                | _ ->
                    raise (new ApplicationException(String.Format(
                        "Invalid binding: tried to bind device {0} with procedure {1}.",
                        src.id, dst)))
            | :? proc as src ->
                if List.exists (fun (x : invokable) -> x.id = dst) types then
                    raise (new ApplicationException(String.Format(
                        "Name override prohibited: tried to overwrite {1} with procedure {0}.",
                        src.id, dst)))
                else let a, b = ret in a, (src, dst) :: b) ([], []) mapping

        let newExtProcs = List.fold (fun ret (ext:invokable) ->
            match ext with
            | :? proc as ext ->
                let rec bindName list =
                    match list with
                    | [] -> ext.id
                    | (ext, ident) :: _ -> ident
                    | _ :: rest -> bindName rest
                let newId = bindName procSubst
                new extProc(newId, ext, deviceSubst) :: ret
            | :? device -> ret) [] programInfo

        newExtProcs @ extProcs) [] load

    assert false

    parsed

and Load file =
    // If it's the first file to load, just load by the path. Otherwise, take a
    // relative path from the file referring it.
    let file = new FileInfo(
        if loadStack.Count = 0 || Path.IsPathRooted(file) then file
        else Path.Combine(loaded.[loadStack.Peek()].Directory.FullName, file))

    let loadID = loaded.IndexOf(file)
    if loadID <> -1 then
        // Look for the stack that does not already loaded.
        if loadStack.Contains(loadID) then
            raise (new ApplicationException("#load caused an include loop."))
        // Should return already loaded program info
        programInfo.[loadID]
    else
        // Try to load the file
        let loadID = loaded.Count
        loaded.Add(file)
        programInfo.Add([])
        loadStack.Push(loadID)
        let parseRet = Parse file
        Debug.Assert((loadID = loadStack.Pop()), "Broken load stack")
        programInfo.[loadID] <- parseRet
        parseRet
