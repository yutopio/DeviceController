module LexTest

open System.Diagnostics
open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser
open Types

let Token string =
    Lexer.token (LexBuffer<char>.FromString(string))

let AssertFailure x : System.Exception =
    try
        let _ = x ()
        Debug.Assert(false)
        raise (new System.NotImplementedException("This statement should not be executed."))
    with exp -> exp

let AssertFailure1 x (y:string) =
    let _ = AssertFailure (fun () -> x y)
    ()

let TestBlockComment () =
    Debug.Assert((Token "/* */") = EOF);
    Debug.Assert((Token "/* hello world */") = EOF);
    Debug.Assert((Token "/* hello *** world */") = EOF);
    Debug.Assert((Token "/**** hello world */") = EOF);
    Debug.Assert((Token "/* hello world ****/") = EOF);
    Debug.Assert((Token "/* hello /*** world */") = EOF);
    ()

let TestRegularStringLiteral () =
    Debug.Assert((Token "\"Test\"") = (STR "Test"));
    Debug.Assert((Token "\"Hello world\"") = (STR "Hello world"));
    AssertFailure1 Token "\"Contains line break\n\"";
    AssertFailure1 Token "\"Not ending string";
    Debug.Assert((Token "\"Contains esc seq \\n\"") = (STR "Contains esc seq \n"));
    Debug.Assert((Token "\"Contains \\\"dbl quote\\\"\"") = (STR "Contains \"dbl quote\""));
    AssertFailure1 Token "\"Illegal esc seq \\a";
    ()

let TestVerbatimStringLiteral () =
    Debug.Assert((Token "@\"Test\"") = (STR "Test"));
    Debug.Assert((Token "@\"Hello world\"") = (STR "Hello world"));
    Debug.Assert((Token "@\"Contains line break\n\"") = (STR "Contains line break\n"));
    AssertFailure1 Token "@\"Not ending string";
    Debug.Assert((Token "@\"Esc seq \\n ignored\"") = (STR "Esc seq \\n ignored"));
    Debug.Assert((Token "@\"Ends with dbl quote \\\"") = (STR "Ends with dbl quote \\"));
    Debug.Assert((Token "@\"Encoded double \"\" dbl quote\"") = (STR "Encoded double \" dbl quote"));
    ()

let Exec () =
    TestBlockComment ();
    TestRegularStringLiteral ();
    TestVerbatimStringLiteral ()
