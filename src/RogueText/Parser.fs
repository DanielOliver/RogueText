﻿module RogueText.Parser


open RogueText.Core
open FLexer.Core
open FLexer.Core.Tokenizer
open FLexer.Core.ClassifierBuilder


/// ######  Lexer words & regex  ######
let DoubleQuote = Consumers.TakeChar '"'
let CurlyLeftBracket = Consumers.TakeChar '{'
let CurlyRightBracket = Consumers.TakeChar '}'
let LeftParentheses = Consumers.TakeChar '('
let RightParentheses = Consumers.TakeChar ')'
let AtSymbol = Consumers.TakeChar '@'
let LeftArrow = Consumers.TakeChar '<'
let RightArrow = Consumers.TakeChar '>'
let ForwardSlash = Consumers.TakeChar '/'

let TRUE = Consumers.TakeWord "true" true
let FALSE = Consumers.TakeWord "false" true
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
    | TextFragment
    | ElementName of string
    | StartTagOpen
    | StartTagEnd
    | StartTagClosed
    | EndTag


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

/// \<elementName>
let AcceptOpenStartTag status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.StartTagOpen LeftArrow status

        let! (elementName, status) = AcceptIdentifier TokenTypes.ElementName status
        
        let! status = Classifier.name TokenTypes.StartTagEnd RightArrow status

        let element: RogueText.Core.Element =
            {   Name = elementName
                Attributes = Map.empty
                Fragments = List.empty
            }

        return element, status
    }

/// \<elementName/>
let AcceptClosedStartTag status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.StartTagOpen LeftArrow status

        let! (tagName, status) = AcceptIdentifier TokenTypes.ElementName status
        
        let! status = Classifier.name TokenTypes.StartTagClosed StartTagClose status
        
        let element: RogueText.Core.Element =
            {   Name = tagName
                Attributes = Map.empty
                Fragments = List.empty
            }

        return element, status
    }

/// \<elementName> some text </>
let rec AcceptElement status continuation =
    Classifiers.sub continuation {
        match AcceptClosedStartTag status Continuation.None with
        | Ok(element, status) ->
            return element |> Tree.Element, status

        | Error _ ->
            let! (element, status) = AcceptOpenStartTag status

            let! (children, status) = ZeroOrMore(status, AcceptElementChildren)
            
            let! status = Classifier.name TokenTypes.EndTag EndTag status

            return { element with Fragments = children } |> Tree.Element, status
    }
    
/// Wraps a text fragment in a tree union.
and AcceptTextFragmentAsTree status continuation =
    Classifiers.sub continuation {
        let! (cleanText, status) = AcceptTextFragment status
        return cleanText |> Tree.Text, status
    }

/// Accepts either an element, text fragment, or word.
and AcceptElementChildren status continuation = 
    Classifiers.sub continuation {
        let! result = PickOne(status, [ AcceptElement; AcceptTextFragmentAsTree ])

        return result
    }
    
let Root status =
    Classifiers.root() {
        let! (value, status) = AcceptElement status
        return value, status
    }

