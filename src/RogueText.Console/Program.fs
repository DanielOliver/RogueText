open FLexer.Core

[<EntryPoint>]
let main argv =
    let sampleText = "public func1( arg1: string, arg0: bool ) <text>hello</>;private elementasdf <item4/>;"

    ClassifierStatus<RogueText.Parser.TokenTypes>.OfString sampleText
    |> RogueText.Parser.Root
    |> (function
        | Ok(sentences, status) ->
            sentences
            |> RogueText.Translator.Translate
        | Error error ->
            error
            |> printfn "%A"
    ) 

    0 // return an integer exit code
