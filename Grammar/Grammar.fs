module Grammar

open System.IO
open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open Types

let Parse file =
    let stream = new FileStream(file, FileMode.Open, FileAccess.Read)
    let reader = new StreamReader(stream)

    // Actual parse phase.
    let stateSave = Lexer.getState ()
    Lexer.initState ()
    let parsed = Parser.compilationUnit Lexer.token (LexBuffer<char>.FromTextReader(reader))
    let finalState = Lexer.getState ()
    Lexer.setState stateSave
    reader.Close()

    // The useful information here is parsed and finalState.


    // should return
    //   device list
    //   proc list


// This is needed to let Lexer to recursively start parsing
Lexer.recursiveParseEntrypoint <- Parse
