module RogueText.Parser


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

let TRUE = Consumers.TakeWord "true" true
let FALSE = Consumers.TakeWord "false" true
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


/// ######  Primitive Parser Functions  ######

// Anything up to Curly Left Bracket or LeftArrow
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

let AcceptIdentifier mapper status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.map mapper IDENTIFIER status
        let identifier = status.ConsumedText

        return identifier, status
    }

// @Identifer   or    @"Complex Identifier"
let AcceptVariable status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.discard AtSymbol status

        let! (variable, status) = PickOne(status, [ AcceptQuotedString TokenTypes.Identifier; AcceptIdentifier TokenTypes.Identifier ])

        return variable, status
    }

