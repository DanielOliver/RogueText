namespace RogueText.Tests

open NUnit.Framework
open RogueText.Core

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
            
    [<Test>]
    member this.TestElement () =
        let emptyElement name = { Element.Name = name; Attributes = Map.empty; Fragments = List.empty  }

        let testFragments = 
            [   true, "<elementName/>", emptyElement "elementName"
                true, "<elementName></>", emptyElement "elementName"
                true, "<elementName>four</>", { emptyElement "elementName" with Fragments = [ SentenceTree.Text "four" ]  }
                false, "<elementName>four", { Element.Name = ""; Attributes = Map.empty; Fragments = List.empty  }
                true, "<elementName> four <subElement234/> </>", { emptyElement "elementName" with Fragments = [ SentenceTree.Text " four "; SentenceTree.Element(emptyElement "subElement234") ]  }
                true, "<elementName> four <subElement234> <sub234/> </> </>", { emptyElement "elementName" with Fragments = [ SentenceTree.Text " four "; SentenceTree.Element({emptyElement "subElement234" with Fragments = [ SentenceTree.Element(emptyElement "sub234") ] }) ] }
                true, "<elementName attribute1  \"nextAttribute\":someValue/>", { emptyElement "elementName" with Attributes = (Map.ofList [ "attribute1", Types.None; "nextAttribute", Types.String "someValue" ]) }
                true, "<elementName> four <subElement234> <sub234 \"Attribute1\"/> </> </>", { emptyElement "elementName" with Fragments = [ SentenceTree.Text " four "; SentenceTree.Element({emptyElement "subElement234" with Fragments = [ SentenceTree.Element({ emptyElement "sub234" with Attributes = Map.ofList [ "Attribute1", Types.None ] }) ] }) ] }
            ] 
            |> List.map (fun (shouldAccept, text, elementToCompare) -> shouldAccept, (text |> FLexer.Core.ClassifierStatus<string>.OfString), elementToCompare)

        for (shouldAccept, fragment, elementToCompare) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptElement fragment FLexer.Core.Continuation.None
        
            match shouldAccept, fragmentResult with
            | true, Ok((SentenceTree.Element element), _) -> Assert.AreEqual(elementToCompare, element)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected to NOT find match: %A" elementToCompare)
            | false, Error _ -> Assert.Pass()
            | _ -> Assert.Fail(sprintf "Expected %A" elementToCompare)
