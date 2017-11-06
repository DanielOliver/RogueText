module RogueText.Tokenizer

open System

let private tagSeparators = 
    [|  "{{>}}"
        ">}}"
        "{{"
        "}}"
    |]
let private tagSeparatorRegexString = "(" + String.Join("|", tagSeparators) + ")"
let private tagSeparatorRegex = Text.RegularExpressions.Regex(tagSeparatorRegexString)


type TagTokenType =
    | CloseTagType
    | OpenTagCloseType
    | OpenTagStartType
    | OpenTagEndType
    | TextType
    | WhitespaceType

[<RequireQualifiedAccess>]
type TagToken =
    | CloseTag
    | OpenTagClose
    | OpenTagStart
    | OpenTagEnd
    | Text of string
    | Whitespace
    static member Identify text =
        match text with
        | "{{>}}" -> TagToken.CloseTag
        | ">}}" -> TagToken.OpenTagEnd
        | "{{" -> TagToken.OpenTagStart
        | "}}" -> TagToken.OpenTagClose
        | x when String.IsNullOrWhiteSpace(x) -> TagToken.Whitespace
        | _ -> TagToken.Text text
    
module TagTokenType =
    let Of token =
        match token with
        | TagToken.CloseTag -> CloseTagType
        | TagToken.OpenTagClose -> OpenTagCloseType
        | TagToken.OpenTagStart -> OpenTagStartType
        | TagToken.OpenTagEnd -> OpenTagEndType
        | TagToken.Text _ -> TextType
        | TagToken.Whitespace -> WhitespaceType

let TokenizeTags text = 
    text 
    |> tagSeparatorRegex.Split 
    |> Array.filter(fun t -> t.Length > 0) 
    |> Array.map TagToken.Identify

let private attributeSeparators = 
    [|  @"['].*?(?<=[^\\])[']"
        @"[A-Za-z][A-Za-z0-9_]*"
        @"="
        @"\s*"
    |]
let private attributeSeparatorRegexString = "(" + String.Join("|", attributeSeparators) + ")"
let private attributeSeparatorRegex = Text.RegularExpressions.Regex(attributeSeparatorRegexString)

[<RequireQualifiedAccess>]
type AttributeToken =
    | Identifier of string
    | Equals

let private cleanAndMapAttributeString text =
    match text with 
    | "=" -> 
        AttributeToken.Equals 
    | _ when text.Length >= 2 && text.StartsWith(@"'") && text.EndsWith(@"'") -> 
        let cleanText = text.Substring(1, text.Length-2).Replace(@"\'", @"'")
        AttributeToken.Identifier cleanText
    | _ ->
        AttributeToken.Identifier text

let TokenizeAttributes text = 
    text 
    |> attributeSeparatorRegex.Split
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map (cleanAndMapAttributeString)
