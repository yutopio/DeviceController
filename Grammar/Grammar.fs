module Grammar

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open Microsoft.FSharp.Text.Lexing
open Error
open Lexer
open Parser
open Types

let mutable internal loaded = new List<FileInfo>();
let mutable internal programInfo = new List<Types.invokable list>();
let mutable internal loadStack = new Stack<int>();

let Reset () =
    loaded <- new List<FileInfo>()
    programInfo <- new List<Types.invokable list>()
    loadStack <- new Stack<int>()

type CommandComparer() =
    inherit Comparer<command>()

    override obj.Compare(x:command, y:command) =
        match x with Command(d1, _, t1, _) ->
        match y with Command(d2, _, t2, _) ->
        let t = t1 - t2
        if t <> 0 then t else d1.id - d2.id

let GetName (x : invokable) = x.name
let HasName name (x : invokable) = x.name = name

let ConvertTimeline (invokables:invokable list) commands =
    // First add all parsed commands with semantics.
    let set = new SortedSet<command>(new CommandComparer())
    ignore(List.fold (fun time (R_Command(dev, arg, timeSpec)) ->
        let dev = List.find (HasName dev) invokables :?> device
        let arg = List.map (Eval >> EvalLiteral) arg
        match timeSpec with
        | None ->
            let invocation = Command(dev, arg, time, time)
            ignore(set.Add(invocation))
            time
        | Some(t1, t2) ->
            let t1 = match t1 with Some t1 -> t1 | None -> time
            let t2 = match t2 with For t2 -> t1 + t2 | To t2 -> t2
            let invocation = Command(dev, arg, t1, t2)
            ignore(set.Add(invocation))
            t2) 0 commands)

    // Verify that the timeline is correctly aligned by time.
    let blockedTime = ref Map.empty
    let last = ref 0
    let commands = List.rev (set.Aggregate([], fun x y -> y :: x))
    match commands with
    | [] -> Time([], 0) // Empty timeline.
    | Command(_, _, t0, _) :: _ ->
        // The first command of the timeline should be after 0ms.
        if t0 < 0 then outRangeTimeSpec t0

        let max = ref 0
        List.iter (fun (Command(dev, _, t1, t2)) ->
            if !max < t2 then max := t2
            if t1 > t2 then invalTimeSpec t1 t2
            else if (match Map.tryFind dev !blockedTime with
                    | Some t0 -> t0 > t1
                    | None -> false) then
                overTimeSpec (dev.ToString()) t1 t2
            else blockedTime := Map.add dev t2 !blockedTime) commands

        // Return the converted timeline.
        Time(commands, !max)

let ConvertProc (invokables : invokable list) (src : procRaw) =
    // Lookup the procedure which we are converting.
    let dst = List.find (HasName src.name) invokables :?> proc

    let rec ConvertProcBodyRaw procBodyRaw =
        match procBodyRaw with
        | R_Time(commands) ->
            ConvertTimeline invokables commands
        | R_Invoke(name, args) ->
            let args = List.map (Eval >> EvalLiteral) args
            Invoke((List.find (HasName name) invokables), args)

    dst.body <- List.map ConvertProcBodyRaw src.body

let rec Parse (file:FileInfo) : invokable list =
    let stream = new FileStream(file.FullName, FileMode.Open, FileAccess.Read)
    let reader = new StreamReader(stream)

    // Try to parse the file.
    let definitions, externalRefs =
        Parser.compilationUnit Lexer.token
            (LexBuffer<char>.FromTextReader(reader))

    // Close the file.
    reader.Close()

    // We should guarantee free of duplicate identity on different invokables.
    let rec checkDuplicateName list =
        match list with
        | [] -> ()
        | elem :: rest ->
            if List.exists ((=) elem) rest then dupName elem
            else checkDuplicateName rest
    checkDuplicateName (List.map GetName definitions)

    // Load external files.
    let extProcs = List.fold(fun extProcs (fileName, subst) ->
        let programInfo = Load fileName

        // Resolve origin invokables in substitution list.
        let mapping = List.map (fun (src, dst) ->
            try (List.find (HasName src) programInfo), dst
            with :? KeyNotFoundException -> noDev src fileName) subst

        // Obtain corresponding device substitutions first.
        let (deviceSubst, procSubst) = List.fold (fun ret (src : invokable, dst) ->
            match src with
            | :? device as src ->
                // Binding of external device. The correspondance should be device.
                match List.find (HasName dst) definitions with
                | :? device as dst -> let a, b = ret in (src, dst) :: a, b
                | _ -> invalBind src.name dst
            | :? proc as src ->
                // Binding of procedure.
                if List.exists (HasName dst) definitions then overBind src.name dst
                else let a, b = ret in a, (src, dst) :: b) ([], []) mapping

        // Include all external procedures with device substitutions enabled.
        // Also some procedures should be renamed according to the substitution list.
        let newExtProcs = List.fold (fun ret (ext:invokable) ->
            match ext with
            | :? proc as ext ->
                let rec bindName list =
                    match list with
                    | [] -> ext.name
                    | (src, ident) :: rest ->
                        if src = ext then ident else bindName rest
                let newName = bindName procSubst
                (new extProc(newName, ext, deviceSubst) :> invokable) :: ret
            | :? device -> ret) [] programInfo

        newExtProcs @ extProcs) [] externalRefs

    // We also need to guarantee that the external references also have no duplicate in the names.
    checkDuplicateName (List.map GetName extProcs)

    // Now make new proc instances for locally defined procedures.
    let localInvokables = List.map (fun (x:invokable) ->
        (match x with
        | :? procRaw as x -> new proc(x.name) :> invokable
        | :? device  -> x)) definitions

    // Replace string identity for the object with its true reference for the concrete object.
    let localProcRaws = List.fold (fun ret (x:invokable) ->
        (match x with
        | :? procRaw as x -> x :: ret
        | :? device  -> ret)) [] definitions
    let availableInvokables = localInvokables @ extProcs
    List.iter (ConvertProc availableInvokables) localProcRaws

    // Finally return the locally defined invokables
    localInvokables

and Load file =
    // If it's the first file to load, just load by the path. Otherwise, take a
    // relative path from the file referring it.
    let file = new FileInfo(
        if loadStack.Count = 0 || Path.IsPathRooted(file) then file
        else Path.Combine(loaded.[loadStack.Peek()].Directory.FullName, file))

    let loadID = loaded.IndexOf(file)
    if loadID <> -1 then
        // Look for the stack that does not already loaded.
        if loadStack.Contains(loadID) then loadLoop file.Name
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
