module Grammar

open System.IO
open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open Types

let Parse file =
    let stream = new FileStream(file, FileMode.Open, FileAccess.Read)
    let reader = new StreamReader(stream)

    let stateSave = Lexer.getState ()
    Lexer.initState ()
    let parsed = Parser.compilationUnit Lexer.token (LexBuffer<char>.FromTextReader(reader))
    Lexer.setState stateSave

    // should return
    //   device list
    //   proc list
