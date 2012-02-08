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
let mutable programInfo = new List<Dictionary<string, Types.invokable>>();
let mutable loadStack = new Stack<int>();
let mutable mainPriority = -1;

let Reset () =
    loaded <- new List<FileInfo>()
    programInfo <- new List<Dictionary<string, Types.invokable>>()
    loadStack <- new Stack<int>()
    priority <- -1

let ChooseDevice (devTable:(string * (int * device) list) list) (ident:Types.ident) =
    let (cnt, name) = ident
    try
        // Look for the device with suitable count
        let (_, devArray) = devTable.FirstOrDefault(fun (x, _) -> x = name)
        let rec MatchDevice (array:(int * device) list) (ret:device option) =
            match array with
            | [] -> ret
            | (id, dev) :: rest -> if id < cnt then MatchDevice rest (Some dev) else (Some dev)
        match MatchDevice devArray None with
        | Some dev -> dev
        | None -> raise (new ApplicationException("Device " + name + " is used before its definition"))
    with :? InvalidOperationException ->
        // No such a device with the specified name
        raise (new ApplicationException("No such a device named " + name))

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
                let invocation = (dev, arg, time, time)
                let _ = set.Add(invocation)
                Inner rest time
            | Some(t1, t2) ->
                let t1 = match t1 with Some t1 -> t1 | None -> time
                let t2 = match t2 with For t2 -> t1 + t2 | To t2 -> t2
                let invocation = (dev, arg, t1, t2)
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

let ValidateProc devTable (procs:Dictionary<string, invokable>) (proc:proc) : Proc =
    let rec Internal procBody ret =
        match procBody with
        | [] -> ret
        | elem :: rest ->
            let elem =
                match elem with
                | Time commands -> T(ValidateTimeline devTable commands)
                | Proc(ident, args) ->
                    let (_, name) = ident
                    let args = List.map (Eval >> EvalLiteral) args
                    I(( if procs.ContainsKey(name) then procs.[name]
                        else (ChooseDevice devTable ident) :> invokable), args)
            Internal rest (ret @ [elem])
    Internal proc.body []

let Parse (file:FileInfo) : Dictionary<string, Types.invokable> =
    let stream = new FileStream(file.FullName, FileMode.Open, FileAccess.Read)
    let reader = new StreamReader(stream)

    // Actual parse phase.
    let savedState = Lexer.getState ()
    Lexer.initState ()
    let parsed = Parser.compilationUnit Lexer.token (LexBuffer<char>.FromTextReader(reader))
    let finalState = Lexer.getState ()
    Lexer.setState savedState
    reader.Close()

    // The useful information here is parsed and finalState.
    let (deviceDefs, externProcs, _, priority, _) = finalState
    if loadStack.Count = 1 then mainPriority <- priority

    // Create a device tables for ease of look up
    let devTable = deviceDefs.GroupBy(
        fun (x:device) -> let (_, name) = x.id in name).Select(
        fun (x:IGrouping<string, device>) -> x.Key, x.Select(
        fun (d:device) -> let (cnt, _) = d.id in (cnt, d)).Aggregate([],
        fun x y -> x @ [y])).Aggregate([], fun x y -> x @ [y])

    // Gather all procedures (internally defined / external reference).
    let procs = parsed.ToDictionary(
        (fun (x:proc) -> let (_, name:string) = x.id in name),
        fun x -> x :> invokable)
    externProcs.Aggregate((), fun _ (x:KeyValuePair<string, extProc>) ->
        try
            procs.Add(x.Key, x.Value :> invokable)
        with _ ->
            raise (new ApplicationException("Duplicate definition procedure: " + x.Key)))

    let convProcs = List.map (ValidateProc devTable procs) parsed
    raise (new NotImplementedException())

let Load file =
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
        programInfo.Add(null)
        loadStack.Push(loadID)
        let parseRet = Parse file
        Debug.Assert((loadID = loadStack.Pop()), "Broken load stack")
        programInfo.[loadID] <- parseRet
        parseRet

// This is needed to let Lexer to recursively start parsing
Lexer.recursiveParseEntrypoint <- Load
