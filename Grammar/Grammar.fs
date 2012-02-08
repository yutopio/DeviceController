module Grammar

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
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
    let (identities, deviceDefs, externProcs, priority, _) = finalState
    if loadStack.Count = 1 then mainPriority <- priority

    // You have to build an identity tables by utilizing identities, deviceDefs, externProcs, parsed.
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
