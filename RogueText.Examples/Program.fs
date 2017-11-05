open System
open System.Text.RegularExpressions

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 


    let input = "{{ text/}} one + 99 and two{{ four}}without seven unless nine{{/}}"

    let output = RogueText.Tokenizer.TokenizeTags input
    printfn "%A" output

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
