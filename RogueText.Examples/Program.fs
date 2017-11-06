open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv = 
    
    let template = RogueText.BackTracker.ReadTemplate "{{ silent attribte='something'}} some {{word>}} informative text{{>}} not helpful{{plural}}unless five equals nine{{>}}"

    printfn "%A" template

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
