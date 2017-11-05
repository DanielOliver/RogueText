module RogueText.Tokenizer

open System

let private tagSeparators = 
    [|  "{{/}}"
        "/}}"
        "{{"
        "}}"
    |]
let private tagSeparatorRegexString = "(" + String.Join("|", tagSeparators) + ")"
let private tagSeparatorRegex = Text.RegularExpressions.Regex(tagSeparatorRegexString)

[<RequireQualifiedAccess>]
type TagToken =
    | CloseExpression
    | OpenTagClose
    | OpenTagStart
    | OpenTagEnd
    | Text of string
    | Whitespace

    static member Identify text =
        match text with
        | "{{/}}" -> TagToken.CloseExpression
        | "/}}" -> TagToken.OpenTagClose
        | "{{" -> TagToken.OpenTagStart
        | "}}" -> TagToken.OpenTagEnd
        | x when String.IsNullOrWhiteSpace(x) -> TagToken.Whitespace
        | _ -> TagToken.Text text

let TokenizeTags text = 
    text 
    |> tagSeparatorRegex.Split 
    |> Array.filter(fun t -> t.Length > 0) 
    |> Array.map TagToken.Identify


