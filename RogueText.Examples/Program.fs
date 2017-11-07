open System
open RogueText
open RogueText.BackTracker

[<EntryPoint>]
let main argv = 
    "{ color='primaryColor' pluralIf='variable1'}{ number='variable1' capitalize bold>} { word='mouse'>} { word='has'>} appeared. Mice are fierce fighters and deadly if swarming.{>}"
    |> RogueText.BackTracker.ReadTemplate 
    |> (function 
        | Ok items -> items |> Seq.iter (printfn "%A")
        | Error err -> err |> printfn "%A")
    0 // return an integer exit code
    
//Tag
//    attributes: 
//        "color", Some "primaryColor" 
//        "pluralIf", Some "variable1"
//    items:   
//        Tag:
//            attributes: 
//                "capitalize", null
//                "bold": null
//                "number", Some "variable1"
//        Contents:
//            " "
//        Tag:
//            attributes:
//                "word", Some "mouse"
//        Contents:
//            " "
//        Tag:
//            attributes:
//                "word", Some "has"     
//        Contents:
//            " appeared. Mice are fierce fighters and deadly if swarming."

