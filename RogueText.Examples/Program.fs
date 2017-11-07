open System
open RogueText

[<EntryPoint>]
let main argv = 

    "{ color='primaryColor' pluralIf='variable1'}{ number='variable1' capitalize>} { word='mouse'>} { word='has'>} appeared. Mice are fierce fighters and deadly if swarming.{>}"
    |> RogueText.BackTracker.ReadTemplate 
    |> printfn "%A"

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
