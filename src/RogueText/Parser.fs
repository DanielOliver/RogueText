module RogueText.Parser


open RogueText.Core
open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core.Classifier
open System


/// ######  Lexer words & regex  ######
let DoubleQuote = Consumers.TakeChar '"'
let CurlyLeftBracket = Consumers.TakeChar '{'
let CurlyRightBracket = Consumers.TakeChar '}'
let LeftBracket = Consumers.TakeChar '['
let RightBracket = Consumers.TakeChar ']'
let LeftParentheses = Consumers.TakeChar '('
let RightParentheses = Consumers.TakeChar ')'
let AtSymbol = Consumers.TakeChar '@'
let Colon = Consumers.TakeChar ':'
let SemiColon = Consumers.TakeChar ';'
let LeftArrow = Consumers.TakeChar '<'
let RightArrow = Consumers.TakeChar '>'
let ForwardSlash = Consumers.TakeChar '/'
let Comma = Consumers.TakeChar ','
let Period = Consumers.TakeChar '.'

let TRUE = Consumers.TakeWord "true" true
let FALSE = Consumers.TakeWord "false" true
let PUBLIC = Consumers.TakeWord "public" true
let PRIVATE = Consumers.TakeWord "private" true
let EndTag = Consumers.TakeWord "</>" true
let StartTagClose = Consumers.TakeWord "/>" true

let StringLiteral = Consumers.TakeRegex "([\\\\][\"]|[^\"])*"
let NumberLiteral = Consumers.TakeRegex "[0-9]([.][0-9){0,1})"
let TextFragment = Consumers.TakeRegex @"([\\][<]|[\\][{]|[^<{])+"
let WHITESPACE = Consumers.TakeRegex @"(\s|[\r\n])+"
let OPTIONAL_WHITESPACE = Consumers.TakeRegex @"(\s|[\r\n])*"
let NumberRegex = Consumers.TakeRegex "([-]){0,1}[0-9]+([.]([0-9])+){0,1}"
let IDENTIFIER = Consumers.TakeRegex "[A-Za-z][A-Za-z0-9]*"


let TYPE_STRING = Consumers.TakeWord "string" true
let TYPE_BOOL = Consumers.TakeWord "bool" true
let TYPE_NUMBER = Consumers.TakeWord "number" true
let TYPE_ARRAY = Consumers.TakeWord "array" true
let TYPE_OPTION = Consumers.TakeWord "option" true

[<RequireQualifiedAccess>]
type TokenTypes =
    | QuotedWord
    | Identifier
    | NumberValue
    | StringValue
    | BooleanValue
    | ArrayStart
    | ArrayEnd
    | ElementStart
    | ElementEnd
    | TextFragment
    | FunctionStart
    | FunctionEnd


// ########### BEGIN UTILITY ###########
let WithDiscardBefore discardFunction function1 status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard discardFunction status
        let! (variable, status) = function1 status
        return variable, status
    }

let WithDiscardAfter discardFunction function1 status continuation =
    Classifiers.sub continuation {
        let! (variable, status) = function1 status        
        let! status = Classifier.discard discardFunction status
        return variable, status
    }

let MapConsumer mapper classifier status continuation =
    Classifiers.sub continuation {
        let! (status: ClassifierStatus<_>) = classifier status
        let value = mapper status.ConsumedText
        return value, status
    }

let NameConsumer name classifier status continuation =
    Classifiers.sub continuation {
        let! (status: ClassifierStatus<_>) = classifier status
        return name, status
    }

    
let MapValue mapper classifier status continuation =
    Classifiers.sub continuation {
        let! (value, status) = classifier status
        return (mapper value), status
    }

/// Local replacement to test out ZeroOrMore with recursive rule.
let ZeroOrMore<'a,'c,'d> (classifier: ClassifierBuilderContinuationFromStatus<'a,'c,'c,'d>) (status: ClassifierStatus<'a>) (continuation: ClassifierBuilderFunction<'a, 'd list, 'c>) =
    let rec RecursiveRule valueList status continuation =
        Classifiers.sub continuation {
            let! (tryValue, status) = ClassifierFunction.ZeroOrOne classifier status
            match tryValue with
            | Some nextValue -> 
                return! RecursiveRule (nextValue :: valueList) status
            | None ->
                return valueList, status
        }
    Classifiers.sub continuation {
        let! (values, status) = RecursiveRule [] status
        return List.rev values, status
    }
// ########### END UTILITY ###########



// ########### BEGIN PRIMITIVES ###########
/// "Quoted Words"
let AcceptQuotedString status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard DoubleQuote status

        let! status = Classifier.name TokenTypes.QuotedWord StringLiteral status
        let quotedWord = status.ConsumedText

        let! status = Classifier.discard DoubleQuote status

        return quotedWord, status
    }

/// A non-quoted continuous alphanumeric word starting with an alpha letter.
let AcceptIdentifier status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.Identifier IDENTIFIER status
        let identifier = status.ConsumedText

        return identifier, status
    }

/// An identifier or quoted word prepended with @
let AcceptVariable status continuation =
    Classifiers.sub continuation {
        let! (variable, status) =
            ClassifierFunction.PickOne [ AcceptQuotedString; AcceptIdentifier ]
            |> WithDiscardBefore AtSymbol
            <| status

        return variable, status
    }
/// Any decimal number
let AcceptNumberValue status continuation = MapConsumer (System.Convert.ToDecimal >> Values.Number) (Classifier.name TokenTypes.NumberValue NumberRegex) status continuation
/// Any quoted string
let AcceptStringValue status continuation = MapValue Values.String (AcceptQuotedString) status continuation
/// True
let AcceptTrue status continuation = NameConsumer true (Classifier.name TokenTypes.BooleanValue TRUE) status continuation
/// False
let AcceptFalse status continuation = NameConsumer false (Classifier.name TokenTypes.BooleanValue FALSE) status continuation
/// True in Values
let AcceptTrueValue status continuation = NameConsumer (Values.Boolean true) (Classifier.name TokenTypes.BooleanValue TRUE) status continuation
/// False in Values
let AcceptFalseValue status continuation = NameConsumer (Values.Boolean false) (Classifier.name TokenTypes.BooleanValue FALSE) status continuation
/// Variable in Values
let AcceptVariableValue status continuation = MapValue Values.Variable AcceptVariable status continuation
/// An array of values
let rec AcceptListValue status continuation = 
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.ArrayStart LeftBracket status

        let! (items, status) = 
            ZeroOrMore (
                ClassifierFunction.PickOne [ AcceptNumberValue; AcceptStringValue; AcceptTrueValue; AcceptFalseValue; AcceptListValue; MapValue Values.Element AcceptElementFunction; MapValue Values.Element AcceptElementText ]
                |> WithDiscardBefore WHITESPACE
            ) status
        
        let! status = Classifier.discard (if List.isEmpty items then OPTIONAL_WHITESPACE else WHITESPACE) status
        let! status = Classifier.name TokenTypes.ArrayEnd RightBracket status
        return items |> Values.List, status
    }
// ########### END PRIMITIVES ###########


// ########### BEGIN LISP ###########
and AcceptLispMethodName status continuation = 
    Classifiers.sub continuation {
        let! (moduleName, status) = 
            ClassifierFunction.ZeroOrOne(
                AcceptIdentifier
                |> WithDiscardAfter Period
            ) status

        let! (functionName, status) = AcceptIdentifier status
        return FunctionCallType.Method(functionName, moduleName), status
    }

and AcceptLispFunctionCall status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.FunctionStart LeftParentheses status
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! (functionType, status) =
            ClassifierFunction.PickOne [ AcceptLispMethodName; MapValue FunctionCallType.FunctionCall AcceptLispFunctionCall ] status
        
        let! (items, status) =
            ZeroOrMore (                
                ClassifierFunction.PickOne [ AcceptNumberValue; AcceptStringValue; AcceptTrueValue; AcceptFalseValue; AcceptListValue; AcceptLispFunctionCallValue; AcceptVariableValue; MapValue Values.Element AcceptElementFunction; MapValue Values.Element AcceptElementText ]
                |> WithDiscardBefore WHITESPACE
            ) status

        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.name TokenTypes.FunctionEnd RightParentheses status
        return {
            FunctionCall.Type = functionType
            FunctionCall.Parameters = items
        }, status
    }

and AcceptLispFunctionCallValue status continuation =
    MapValue Values.FunctionCall AcceptLispFunctionCall status continuation

and AcceptAssignmentValue status continuation =
        ClassifierFunction.PickOne [ AcceptLispFunctionCallValue; AcceptNumberValue; AcceptStringValue; AcceptTrueValue; AcceptFalseValue; AcceptVariableValue ] status continuation
// ########### END LISP ###########


// ########### BEGIN ATTRIBUTE ###########
and AcceptAttributeAssignment status continuation =
    Classifiers.sub continuation {
        let! (attributeName, status) = ClassifierFunction.PickOne [ AcceptQuotedString; AcceptIdentifier ] status
        
        let! (value, status) =
            ClassifierFunction.ZeroOrOne (
                AcceptAssignmentValue
                |> WithDiscardBefore OPTIONAL_WHITESPACE
                |> WithDiscardBefore Colon
                |> WithDiscardBefore OPTIONAL_WHITESPACE
            ) status

        return (attributeName, value), status
    }
// ########### END ATTRIBUTE ###########


// ########### BEGIN ELEMENT ###########
and AcceptFragmentText status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.TextFragment TextFragment status
        let cleanedText = status.ConsumedText.Replace(@"\{", "{").Replace(@"\<", "<")
        return cleanedText, status
    }

and AcceptElementText status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.ElementStart LeftArrow status
        let! status = Classifier.discard AtSymbol status

        let! (attributes, status) =
            ZeroOrMore (
                AcceptAttributeAssignment
                |> WithDiscardBefore WHITESPACE
            ) status
        
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.name TokenTypes.ElementStart RightArrow status

        let! (fragments, status) = AcceptFragmentsTree status

        let! status = Classifier.name TokenTypes.ElementEnd EndTag status

        return {
            Element.Attributes = attributes |> Map.ofList
            Element.Fragments = fragments
            Element.ElementType = ElementType.Text
        }, status
    }

and AcceptElementFunction status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.ElementStart LeftArrow status

        let! (functionCall, status) =
            ClassifierFunction.PickOne
                [
                    ClassifierFunction.PickOne 
                        [ 
                            AcceptQuotedString
                            AcceptIdentifier 
                        ] |> MapValue (fun methodName -> { FunctionCall.Type = FunctionCallType.Method(methodName, None); Parameters = [] } )
                    AcceptLispFunctionCall 
                ]
            |> MapValue ElementType.FunctionCall
            <| status

        let! (attributes, status) =
            ZeroOrMore (
                AcceptAttributeAssignment
                |> WithDiscardBefore WHITESPACE
            ) status
        
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! status = Classifier.name TokenTypes.ElementStart StartTagClose status

        return {
            Element.Attributes = attributes |> Map.ofList
            Element.ElementType = functionCall
            Element.Fragments = []
        }, status
    }

and AcceptFragmentsTree status continuation =
    ZeroOrMore (
        ClassifierFunction.PickOne
            [
                MapValue SentenceTree.Text AcceptFragmentText
                MapValue SentenceTree.Element AcceptElementFunction
                MapValue SentenceTree.Element AcceptElementText 
            ]
    ) status continuation
// ########### END ELEMENT ###########
