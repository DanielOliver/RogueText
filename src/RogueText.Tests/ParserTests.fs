namespace RogueText.Tests

open NUnit.Framework

[<ParserTests>]
type ParserTests () =

    [<Test>]
    member this.TestTextFragment () =
        let testFragments = 
            [   true, "textFragment", "textFragment"
                true, "textFragment{", "textFragment"
                true, "textFragment<", "textFragment"
                true, @"textFragment\{one", "textFragment{one"
                false, @"<textFragment\{one", ""
                false, @"{textFragment\{one", ""
            ] 
            |> List.map (fun (shouldAccept, text, textToAccept) -> shouldAccept, (text |> FLexer.Core.ClassifierStatus<string>.OfString), textToAccept)

        for (shouldAccept, fragment, textToAccept) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptTextFragment fragment FLexer.Core.Continuation.None
        
            match shouldAccept, fragmentResult with
            | true, Ok(cleanText, _) -> Assert.AreEqual(textToAccept, cleanText)
            | true, Error _ -> Assert.Fail(sprintf "Expected \"%s\" to match \"%s\"." fragment.Remainder textToAccept)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected \"%s\" to NOT find match." fragment.Remainder)
            | false, Error _ -> Assert.Pass()

    [<Test>]
    member this.TestIdentifier () =
        let testFragments = 
            [   true, "@textFragment", "textFragment"
                false, "textFragment", ""
                false, "text Fragment", ""
                true, "@text   Fragment", "text"
                true, "@\"text   Fragment\"", "text   Fragment"
                true, "@\"  text  345____  Fragment\"", "  text  345____  Fragment"
                false, "@ text Fragment", ""
            ] 
            |> List.map (fun (shouldAccept, text, textToAccept) -> shouldAccept, (text |> FLexer.Core.ClassifierStatus<string>.OfString), textToAccept)

        for (shouldAccept, fragment, textToAccept) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptVariable fragment FLexer.Core.Continuation.None
        
            match shouldAccept, fragmentResult with
            | true, Ok(cleanText, _) -> Assert.AreEqual(textToAccept, cleanText)
            | true, Error _ -> Assert.Fail(sprintf "Expected \"%s\" to match \"%s\"." fragment.Remainder textToAccept)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected \"%s\" to NOT find match." fragment.Remainder)
            | false, Error _ -> Assert.Pass()

