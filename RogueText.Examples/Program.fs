open System
open RogueText

[<EntryPoint>]
let main argv = 

    "four and a half {{ silent attribute='something'}} some {{word>}} informative text {{}} maybe {{nested2>}} {{>}} OR IS IT?{{>}} not helpful{{plural}}unless five equals nine{{>}}"
    //|> Tokenizer.TokenizeTags
    |> RogueText.BackTracker.ReadTemplate 
    |> printfn "%A"

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
