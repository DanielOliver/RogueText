﻿open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv = 

    let input = "{{ text>}} one + 99 and two{{ four}}without seven unless nine{{>}}"

    let output = RogueText.Tokenizer.TokenizeTags input
    printfn "%A" output


    let input = @"What what what 'and how\' unless' "
    let output = RogueText.Tokenizer.TokenizeAttributes input
    printfn "%A" output

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
