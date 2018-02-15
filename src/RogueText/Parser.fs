module RogueText.Parser


open RogueText.Core
open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core.ClassifierBuilder
open FLexer.Core.Classifier


/// ######  Lexer words & regex  ######
let DoubleQuote = Consumers.TakeChar '"'
let CurlyLeftBracket = Consumers.TakeChar '{'
let CurlyRightBracket = Consumers.TakeChar '}'
let LeftParentheses = Consumers.TakeChar '('
let RightParentheses = Consumers.TakeChar ')'
let AtSymbol = Consumers.TakeChar '@'
let Colon = Consumers.TakeChar ':'
let SemiColon = Consumers.TakeChar ';'
let LeftArrow = Consumers.TakeChar '<'
let RightArrow = Consumers.TakeChar '>'
let ForwardSlash = Consumers.TakeChar '/'

let TRUE = Consumers.TakeWord "true" true
let FALSE = Consumers.TakeWord "false" true
let PUBLIC = Consumers.TakeWord "public" true
let PRIVATE = Consumers.TakeWord "private" true
let EndTag = Consumers.TakeWord "</>" true
let StartTagClose = Consumers.TakeWord "/>" true

let StringLiteral = Consumers.TakeRegex "([\\\\][\"]|[^\"])*"
let TextFragment = Consumers.TakeRegex @"([\\][<]|[\\][{]|[^<{])+"
let WHITESPACE = Consumers.TakeRegex @"(\s|[\r\n])+"
let OPTIONAL_WHITESPACE = Consumers.TakeRegex @"(\s|[\r\n])*"
let NumberRegex = Consumers.TakeRegex "([-]){0,1}[0-9]+([.]([0-9])+){0,1}"
let IDENTIFIER = Consumers.TakeRegex "[A-Za-z][A-Za-z0-9]*"


[<RequireQualifiedAccess>]
type TokenTypes =
    | Identifier of string
    | FunctionName of string
    | AttributeName of string
    | TextFragment
    | ElementName of string
    | AttributeLabel of string
    | StartTagOpen
    | StartTagEnd
    | StartTagClosed
    | EndTag
    | AccessModifier of AccessModifier


/// ######  Primitive Parser Functions  ######

/// Any text up to Curly Left Bracket or LeftArrow
let AcceptTextFragment status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.TextFragment TextFragment status
        let cleanedText = status.ConsumedText.Replace(@"\{", "{").Replace(@"\<", "<")
        return cleanedText, status
    }

/// "Quoted Words"
let AcceptQuotedString mapper status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard DoubleQuote status

        let! status = Classifier.map mapper StringLiteral status
        let quotedWord = status.ConsumedText

        let! status = Classifier.discard DoubleQuote status

        return quotedWord, status
    }

/// A non-quoted continuous alphanumeric word starting with an alpha letter.
let AcceptIdentifier mapper status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.map mapper IDENTIFIER status
        let identifier = status.ConsumedText

        return identifier, status
    }

/// An identifier or quoted word prepended with @
let AcceptVariable status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard AtSymbol status

        let! (variable, status) = PickOne(status, [ AcceptQuotedString TokenTypes.Identifier; AcceptIdentifier TokenTypes.Identifier ])

        return variable, status
    }

/// An attribute with a value (or no value), and a prepended space.
let AcceptWhitespaceBeforeAttribute status continuation =
    Classifiers.sub continuation {
        let! (attributeLabel, status) = PickOne(status, [ AcceptQuotedString TokenTypes.Identifier; AcceptIdentifier TokenTypes.AttributeName ])

        match Classifier.discard Colon status with
        | Ok(status) ->
            let! (attributeValue, status) = PickOne(status, [ AcceptQuotedString TokenTypes.Identifier; AcceptIdentifier TokenTypes.Identifier ])

            return (attributeLabel, Values.String attributeValue), status

        | Error _ ->

            return (attributeLabel, Values.None), status
    }

/// Multiple attributes with a value (or no value), and a prepended space.
let AcceptMultipleAttributes status continuation =
    Classifiers.sub continuation {
        let! (items, status) = ZeroOrMore(status, AcceptWhitespaceBeforeAttribute)

        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        return items |> Map.ofList, status
    }

/// \<elementName>
let AcceptOpenStartTag status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.StartTagOpen LeftArrow status

        let! (elementName, status) = AcceptIdentifier TokenTypes.ElementName status

        let! (attributes, status) = AcceptMultipleAttributes status
        
        let! status = Classifier.name TokenTypes.StartTagEnd RightArrow status

        let element: RogueText.Core.Element =
            {   Name = elementName
                Attributes = attributes
                Fragments = List.empty
            }

        return element, status
    }

/// \<elementName/>
let AcceptClosedStartTag status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.StartTagOpen LeftArrow status

        let! (tagName, status) = AcceptIdentifier TokenTypes.ElementName status
        
        let! (attributes, status) = AcceptMultipleAttributes status
        
        let! status = Classifier.name TokenTypes.StartTagClosed StartTagClose status
        
        let element: RogueText.Core.Element =
            {   Name = tagName
                Attributes = attributes
                Fragments = List.empty
            }

        return element, status
    }

/// \<elementName> some text </>
let rec AcceptElement status continuation =
    Classifiers.sub continuation {
        match AcceptClosedStartTag status Continuation.None with
        | Ok(element, status) ->
            return element |> SentenceTree.Element, status

        | Error _ ->
            let! (element, status) = AcceptOpenStartTag status

            let! (children, status) = ZeroOrMore(status, AcceptElementChildren)
            
            let! status = Classifier.name TokenTypes.EndTag EndTag status

            return { element with Fragments = List.rev children } |> SentenceTree.Element, status
    }
    
/// Wraps a text fragment in a tree union.
and AcceptTextFragmentAsTree status continuation =
    Classifiers.sub continuation {
        let! (cleanText, status) = AcceptTextFragment status
        return cleanText |> SentenceTree.Text, status
    }

/// Accepts either an element, text fragment, or word.
and AcceptElementChildren status continuation = 
    Classifiers.sub continuation {
        let! result = PickOne(status, [ AcceptElement; AcceptTextFragmentAsTree ])

        return result
    }

/// public
let AcceptPublicModifier status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name (AccessModifier.Public |> TokenTypes.AccessModifier) PUBLIC status
        return AccessModifier.Public, status
    }

/// private
let AcceptPrivateModifier status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name (AccessModifier.Private |> TokenTypes.AccessModifier) PRIVATE status
        return AccessModifier.Private, status
    }

/// public functionName(argument1: type) <element1>someText</element1>
let AcceptFunction status continuation =
    Classifiers.sub continuation {
        let! (accessModifier, status) = PickOne(status, [ AcceptPublicModifier; AcceptPrivateModifier ])
        
        let! status = Classifier.discard WHITESPACE status
        
        let! (functionName, status) = PickOne(status, [ AcceptQuotedString TokenTypes.Identifier; AcceptIdentifier TokenTypes.AttributeName ])
        
        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! (sentence, status) = AcceptElement status

        let! status = Classifier.discard OPTIONAL_WHITESPACE status

        let! status = Classifier.discard SemiColon status

        return {
            SentenceFunction.AccessModifier = accessModifier
            SentenceFunction.Name = functionName
            SentenceFunction.Sentence = sentence
            SentenceFunction.Arguments = Array.empty
        }, status
    }

/// Accept whitespace before functions
let AcceptWhitespaceBeforeFunction status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! result = AcceptFunction status
        return result
    }

/// Accept list of functions
let AcceptFunctionList status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        let! (functions, status) = ZeroOrMore(status, AcceptWhitespaceBeforeFunction)
        let! status = Classifier.discard OPTIONAL_WHITESPACE status
        return List.rev functions, status
    }
    
let Root status =
    Classifiers.root() {
        let! (value, status) = AcceptFunctionList status
        return value, status
    }

