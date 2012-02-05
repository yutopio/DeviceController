module Grammar

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open Types

let Parse reader =
    Parser.compilationUnit Lexer.token (LexBuffer<char>.FromTextReader(reader))

let Preprocess line =
    Parser.ppDirective Lexer.preprocToken (LexBuffer<char>.FromString(line))
