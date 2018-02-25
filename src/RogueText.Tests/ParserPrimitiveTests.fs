namespace RogueText.Tests

open NUnit.Framework
open RogueText.Core

[<ParserPrimitiveTests>]
type ParserPrimitiveTests () =
    let ToClassifier text = FLexer.Core.ClassifierStatus<string>.OfString text
    let Continuation = FLexer.Core.Continuation.None


    [<Test>]
    member this.TestQuotedStrings () =
        let testFragments = 
            [   true, "\"quotedString\"", "quotedString"
                false, "\"quotedString", "quotedString"
                true, "\" quoted Strings are the best \"", " quoted Strings are the best "
            ] 
            |> List.map (fun (shouldAccept, text, textToAccept) -> shouldAccept, (text |> ToClassifier), textToAccept)

        for (shouldAccept, fragment, textToAccept) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptQuotedString fragment Continuation
        
            match shouldAccept, fragmentResult with
            | true, Ok(cleanText, _) -> Assert.AreEqual(textToAccept, cleanText)
            | true, Error _ -> Assert.Fail(sprintf "Expected \"%s\" to match \"%s\"." fragment.Remainder textToAccept)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected \"%s\" to NOT find match." fragment.Remainder)
            | false, Error _ -> Assert.Pass()
            
    [<Test>]
    member this.TestValues () =
        let testFragments = 
            [   true, "[ \"quotedString\" false true 0.123 [ TRUE FALSE ] ]", Values.List [ Values.String "quotedString"; Values.Boolean false; Values.Boolean true; Values.Number 0.123M; Values.List [ Values.Boolean true; Values.Boolean false ] ]
            ] 
            |> List.map (fun (shouldAccept, text, textToAccept) -> shouldAccept, (text |> ToClassifier), textToAccept)

        for (shouldAccept, fragment, dataToAccept) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptListValue fragment Continuation
        
            match shouldAccept, fragmentResult with
            | true, Ok(cleanText, _) -> Assert.AreEqual(dataToAccept, cleanText)
            | true, Error _ -> Assert.Fail(sprintf "Expected \"%s\" to match \"%A\"." fragment.Remainder dataToAccept)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected \"%s\" to NOT find match." fragment.Remainder)
            | false, Error _ -> Assert.Pass()
            
    [<Test>]
    member this.TestLispExpressions () =
        let testFragments = 
            [   true, "(List.values 0.1234)", { Type = FunctionCallType.Method ("values", Some "List"); Parameters = [ Values.Number 0.1234M ] }

                true, "(List.values 0.1234 (mapSome [] \"quoted String\" ))", { Type = FunctionCallType.Method ("values", Some "List"); Parameters = [ Values.Number 0.1234M; (Values.FunctionCall { Type = FunctionCallType.Method ("mapSome", None); Parameters = [ Values.List []; Values.String "quoted String" ] } ) ] }
                true, "(List.values 0.1234 @arg1)", { Type = FunctionCallType.Method ("values", Some "List"); Parameters = [ Values.Number 0.1234M; Values.Variable "arg1" ] }
                false, "(\"one\" 0.1234 @arg1)", { Type = FunctionCallType.Method ("", None); Parameters = [] }
                true, "((List.map (add 1)) 0.1234)", { Type = FunctionCallType.FunctionCall { FunctionCall.Type = FunctionCallType.Method("map", Some "List"); FunctionCall.Parameters = [ Values.FunctionCall { FunctionCall.Type = FunctionCallType.Method("add", None); FunctionCall.Parameters = [] } ] }; Parameters = [ Values.Number 0.1234M ] }
            ] 
            |> List.map (fun (shouldAccept, text, textToAccept) -> shouldAccept, (text |> ToClassifier), textToAccept)

        for (shouldAccept, fragment, dataToAccept) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptLispFunctionCall fragment Continuation
        
            match shouldAccept, fragmentResult with
            | true, Ok(cleanText, _) -> Assert.AreEqual(dataToAccept, cleanText)
            | true, Error _ -> Assert.Fail(sprintf "Expected \"%s\" to match \"%A\"." fragment.Remainder dataToAccept)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected \"%s\" to NOT find match." fragment.Remainder)
            | false, Error _ -> Assert.Pass()
            
    [<Test>]
    member this.TestFragments () =
        let testFragments = 
            [   true, "some text <someElement/>", [ SentenceTree.Text "some text "; SentenceTree.Element { Element.ElementType = ElementType.FunctionCall { FunctionCall.Type = FunctionCallType.Method("someElement", None); FunctionCall.Parameters = [] }; Element.Attributes = Map.empty; Element.Fragments = [] } ]
                true, "<@>some text</> other text <someElement/>", [ SentenceTree.Element { Element.ElementType = ElementType.Text; Element.Attributes = Map.empty; Element.Fragments = [ SentenceTree.Text "some text" ] }; SentenceTree.Text " other text "; SentenceTree.Element { Element.ElementType = ElementType.FunctionCall { FunctionCall.Type = FunctionCallType.Method("someElement", None); FunctionCall.Parameters = [] }; Element.Attributes = Map.empty; Element.Fragments = [] } ]
                true, "<@ color=red  attribute1=@variable1>some text</>", [ SentenceTree.Element { Element.ElementType = ElementType.Text; Element.Attributes = Map.ofList [ "color", Some <| Values.String "red"; "attribute1", Some <| Values.Variable "variable1" ]; Element.Fragments = [ SentenceTree.Text "some text" ] } ]
                true, "<(component1 0.12343)/>", [ SentenceTree.Element { Element.ElementType = ElementType.FunctionCall { FunctionCall.Type = FunctionCallType.Method("component1", None); FunctionCall.Parameters = [ Values.Number 0.12343M ] }; Element.Attributes = Map.empty; Element.Fragments = [] } ]
            ] |> List.map (fun (shouldAccept, text, textToAccept) -> shouldAccept, (text |> ToClassifier), textToAccept)

        for (shouldAccept, fragment, dataToAccept) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptFragmentsTree fragment Continuation
        
            match shouldAccept, fragmentResult with
            | true, Ok(cleanText, _) -> Assert.AreEqual(dataToAccept, cleanText)
            | true, Error _ -> Assert.Fail(sprintf "Expected \"%s\" to match \"%A\"." fragment.Remainder dataToAccept)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected \"%s\" to NOT find match." fragment.Remainder)
            | false, Error _ -> Assert.Pass()
