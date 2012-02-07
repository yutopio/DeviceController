module Grammar

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open Types

let DebugToken lexbuf =
    System.Diagnostics.Debug.WriteLine(Lexer.lexeme lexbuf)
    Lexer.token lexbuf

let Parse reader =
    Parser.InitState ()
    Parser.compilationUnit DebugToken (LexBuffer<char>.FromTextReader(reader))

let Preprocess line =
    Parser.ppDirective Lexer.preprocToken (LexBuffer<char>.FromString(line))
