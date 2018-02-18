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
            [   true, "(List.values 0.1234)", Values.FunctionCall { Name = "values"; Module = Some "List"; Parameters = [ Values.Number 0.1234M ] }
                true, "(List.values 0.1234 (mapSome [] \"quoted String\" ))", Values.FunctionCall { Name = "values"; Module = Some "List"; Parameters = [ Values.Number 0.1234M; (Values.FunctionCall { Name = "mapSome"; Module = None; Parameters = [ Values.List []; Values.String "quoted String" ] } ) ] }
                true, "(List.values 0.1234 @arg1)", Values.FunctionCall { Name = "values"; Module = Some "List"; Parameters = [ Values.Number 0.1234M; Values.Variable "arg1" ] }
                false, "(\"one\" 0.1234 @arg1)", Values.Boolean true
            ] 
            |> List.map (fun (shouldAccept, text, textToAccept) -> shouldAccept, (text |> ToClassifier), textToAccept)

        for (shouldAccept, fragment, dataToAccept) in testFragments do

            let fragmentResult = RogueText.Parser.AcceptLispFunctionCall fragment Continuation
        
            match shouldAccept, fragmentResult with
            | true, Ok(cleanText, _) -> Assert.AreEqual(dataToAccept, cleanText)
            | true, Error _ -> Assert.Fail(sprintf "Expected \"%s\" to match \"%A\"." fragment.Remainder dataToAccept)
            | false, Ok(_, _) -> Assert.Fail(sprintf "Expected \"%s\" to NOT find match." fragment.Remainder)
            | false, Error _ -> Assert.Pass()