
type Example = RogueText.Print<"one">


[<EntryPoint>]
let main argv = 
    "{ color='primaryColor' pluralIf='@variable1'}{ number='variable1' capitalize bold>} { word='mouse'>} { word='has'>} appeared. Mice are fierce fighters and deadly if swarming.{>} Mice are {@ something? }"
    |> RogueText.BackTracker.ReadTemplate
    |> (function 
        | Ok ok -> 
            ok
            |> Seq.iter (printfn "%A")

        | Error err -> err |> printfn "%A")
        
    Example.show |> printfn "%s"
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
    