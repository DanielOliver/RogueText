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
            ClassifierFunction.ZeroOrMore (                
                ClassifierFunction.PickOne [ AcceptNumberValue; AcceptStringValue; AcceptTrueValue; AcceptFalseValue; AcceptListValue ]            
                |> WithDiscardBefore WHITESPACE
            ) status
        
        let! status = Classifier.discard (if List.isEmpty items then OPTIONAL_WHITESPACE else WHITESPACE) status
        let! status = Classifier.name TokenTypes.ArrayEnd RightBracket status
        return items |> Values.List, status
    }
// ########### END PRIMITIVES ###########

// ########### BEGIN LISP ###########
let rec AcceptLispExpression status continuation =
    Classifiers.sub continuation {
        let! status = Classifier.name TokenTypes.ArrayStart LeftParentheses status
        let! status = Classifier.name TokenTypes.ArrayStart OPTIONAL_WHITESPACE status
        let! (moduleName, status) = 
            ClassifierFunction.ZeroOrOne(
                AcceptIdentifier
                |> WithDiscardAfter Period
            ) status

        let! (functionName, status) = AcceptIdentifier status
        
        let! (items, status) = 
            ClassifierFunction.ZeroOrMore (                
                ClassifierFunction.PickOne [ AcceptNumberValue; AcceptStringValue; AcceptTrueValue; AcceptFalseValue; AcceptListValue; AcceptLispExpression; AcceptVariableValue ]
                |> WithDiscardBefore WHITESPACE
            ) status

        let! status = Classifier.name TokenTypes.ArrayStart OPTIONAL_WHITESPACE status
        let! status = Classifier.name TokenTypes.ArrayStart RightParentheses status
        return {
            FunctionCall.Module = moduleName
            FunctionCall.Name = functionName
            FunctionCall.Parameters = items
        } |> Values.FunctionCall, status
    }

// ########### END LISP ###########
